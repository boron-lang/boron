use crate::errors::{MainNoGenerics, MainNoParams, MainRetNotAUnit, NoMainFunction};
use crate::prelude::*;
use boron_analysis::results::BuiltInResults;
use boron_analysis::validator::validate_comptime;
use boron_analysis::{InferTy, TypeTable, expand_builtins, typeck_hir};
use boron_codegen::run_codegen;
use boron_compiler::CompilerBuild;
use boron_hir::hir::Hir;
use boron_hir::lower::lower_to_hir;
use boron_ir::{Ir, IrLowerer};
use boron_parser::module::{Module, Modules};
use boron_parser::parser::errors::ModuleNotFound;
use boron_parser::parser::parse;
use boron_resolver::{DefId, ResolveVisitor, Resolver};
use boron_source::source_file::SourceFileId;
use boron_thir::{Thir, ThirLowerer};
use std::process::exit;
use std::time::Instant;

pub struct CompilationUnit<'ctx> {
  pub entry_point: SourceFileId,
  pub sess: &'ctx Session,
  pub modules: Modules,
  pub resolver: Resolver,
  pub hir: Option<Hir>,
  pub typeck: Option<TypeTable>,
  pub builtin_results: Option<BuiltInResults>,
  pub thir: Option<Thir>,
  pub ir: Option<Ir>,
  pub main_function: Option<DefId>,
}

impl<'ctx> CompilationUnit<'ctx> {
  pub fn new(entry_point: SourceFileId, sess: &'ctx Session) -> Self {
    Self {
      entry_point,
      sess,
      modules: Modules::new(),
      resolver: Resolver::new(),
      hir: None,
      typeck: None,
      builtin_results: None,
      thir: None,
      ir: None,
      main_function: None,
    }
  }

  pub fn check(&mut self) {
    let Some(_entrypoint) = self.file_to_module(self.entry_point) else {
      return;
    };

    self.resolver.build_import_graph(&self.modules);
    if self.run_step("Name resolution", |this| this.resolve_names()) {
      return;
    }
    if self.run_step("HIR lowering", |this| this.lower_to_hir()) {
      return;
    }
    if self.run_step("Comptime validation", |this| this.validate_comptime()) {
      return;
    }

    if self.run_step("Type inference and checking", |this| this.typeck()) {
      return;
    }
    if self.run_step("Built-in expanding", |this| this.expand_builtins()) {
      return;
    }
    if self.run_step("THIR lowering", |this| this.lower_to_thir()) {
      return;
    }
    self.find_main_function();

    let _ = self.run_step("IR lowering", |this| this.lower_to_ir());
  }

  fn find_main_function(&mut self) {
    if self.sess.is_lib() {
      return;
    }
    let Some(thir) = &self.thir else {
      return;
    };

    let main = thir.functions.iter().find(|func| func.name.text() == "main");

    if let Some(main) = main {
      if !main.generics.is_empty() {
        self.sess.dcx().emit(MainNoGenerics { span: main.generics.span });
      }
      if !main.params.is_empty() {
        let span = match (main.params.first(), main.params.last()) {
          (Some(first), Some(last)) => first.span.to(last.span),
          (Some(first), None) => first.span,
          _ => unreachable!(),
        };

        self.sess.dcx().emit(MainNoParams { span });
      }

      if !matches!(main.return_type, InferTy::Unit(_)) {
        self.sess.dcx().emit(MainRetNotAUnit { span: main.return_type.span() });
      }

      self.main_function = Some(main.def_id);
    } else {
      self.sess.dcx().emit(NoMainFunction { pkg: self.sess.config.name.clone() });
    }
  }

  pub fn build(&mut self) -> Result<()> {
    self.check();
    if self.sess().create_output_dir().is_none() {
      self.sess().dcx().bug("failed to create output directory");
    }

    let Some(ir) = &self.ir else {
      return Ok(());
    };

    let start = Instant::now();
    if let Err(err) = run_codegen(self.sess, ir, self.main_function) {
      self.emit_errors();
      return Err(err);
    }
    self.sess.store_timing("LLVM codegen", start.elapsed());

    let linking_start = Instant::now();
    match self.link() {
      Ok(path) => {
        self.sess.store_timing("Linking", linking_start.elapsed());
        Ok(())
      }
      Err(err) => Err(err),
    }
  }

  fn link(&mut self) -> Result<PathBuf> {
    let mut build = CompilerBuild::new(self.sess)?;
    build.add_source(self.sess.obj_file());

    build.compile(self.sess.config.name.clone())
  }

  fn lower_to_thir(&mut self) {
    let Some(hir) = &self.hir else { return };
    let Some(typeck) = &self.typeck else {
      return;
    };
    let Some(builtin_results) = &self.builtin_results else {
      return;
    };

    self.thir = Some(
      ThirLowerer::new(hir, &self.resolver, self.sess.dcx(), typeck, builtin_results)
        .lower(),
    );
  }

  fn lower_to_ir(&mut self) {
    let Some(hir) = &self.hir else { return };
    let Some(thir) = &self.thir else { return };
    let Some(typeck) = &self.typeck else {
      return;
    };

    self.ir = Some(IrLowerer::new(hir, thir, typeck).lower());
  }

  fn expand_builtins(&mut self) {
    let Some(hir) = &self.hir else { return };
    let Some(typeck) = &self.typeck else {
      return;
    };

    self.builtin_results = Some(expand_builtins(self.sess, &self.resolver, typeck, hir));
  }

  fn validate_comptime(&self) {
    let Some(hir) = &self.hir else { return };
    let dcx = self.sess.dcx();

    validate_comptime(hir, dcx, &self.resolver);
  }

  fn typeck(&mut self) {
    let Some(hir) = &self.hir else { return };
    let table = typeck_hir(hir, self.sess, &self.resolver);

    self.typeck = Some(table);
  }

  fn resolve_names(&self) {
    ResolveVisitor::resolve_modules(&self.resolver, &self.modules, self.sess);
  }

  fn lower_to_hir(&mut self) {
    let dcx = self.sess.dcx();
    let hir = lower_to_hir(&self.resolver, &self.modules, dcx);
    self.hir = Some(hir);
  }

  pub fn sess(&self) -> &Session {
    self.sess
  }

  fn run_step<F>(&mut self, name: &str, step: F) -> bool
  where
    F: FnOnce(&mut Self),
  {
    debug!("starting {name}");
    let start = Instant::now();
    step(self);
    let end = start.elapsed();

    self.sess.store_timing(name, end);
    self.emit_errors()
  }

  fn file_to_module(&mut self, id: SourceFileId) -> Option<SourceFileId> {
    let dcx = self.sess.dcx();

    let source_file = self.sess.sources().get(id).unwrap_or_else(|| {
      dcx.bug(format!("Source file {id:?} not found"));
      unreachable!();
    });

    let mut lexer = Lexer::new(source_file.value(), dcx);
    let tokens = lexer.tokenize().unwrap_or_else(|| {
      dcx.bug(format!("lexer made no progress for source file {id:?}"));
      unreachable!();
    });
    if self.emit_errors() {
      return None;
    }

    let node = parse(tokens, dcx);
    if self.emit_errors() {
      return None;
    }

    let module = Module::new(id, node);
    self.modules.add(module);

    let files_to_process: Vec<_> = {
      let this = self.modules.get_unchecked(id);
      let dcx = self.sess.dcx();
      let mut files = Vec::new();

      for module in &this.node.discover_modules {
        let full_path = module.construct_file(self.sess.root(), source_file.path());

        let Some(path) = full_path else {
          dcx.emit(ModuleNotFound { module: module.clone(), span: module.span });
          continue;
        };

        if !path.exists() {
          dcx.emit(ModuleNotFound { module: module.clone(), span: module.span });
          continue;
        }

        if self.sess.sources().get_by_path(&path).is_some() {
          continue;
        }

        let contents = fs_err::read_to_string(path.clone());
        let Ok(contents) = contents else {
          dcx.bug(format!("Failed to read contents of {}", path.display()));
          continue;
        };

        let file_id = self.sess.sources().add(contents, path);
        self.sess().graph().add_discovered_relation(id, file_id);
        files.push(file_id);
      }

      files
    };

    for file_id in files_to_process {
      let _ = self.file_to_module(file_id);
    }

    self.emit_errors();

    Some(id)
  }

  fn emit_errors(&self) -> bool {
    let emitted = self.sess.dcx().emit_all();
    if emitted > 0 {
      error!(
        "stopping compilation due to {emitted} {}",
        if emitted == 1 { "error" } else { "errors" }
      );
    }

    if emitted > 0 {
      self.sess.dcx().flush_to_stderr();
      if !self.sess.is_test() {
        exit(1)
      }
    }

    emitted > 0
  }
}
