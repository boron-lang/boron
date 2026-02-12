use crate::prelude::*;
use boron_analysis::results::BuiltInResults;
use boron_analysis::validator::validate_comptime;
use boron_analysis::{expand_builtins, typeck_hir, TypeTable};
use boron_codegen::run_codegen;
use boron_hir::hir::Hir;
use boron_hir::lower::lower_to_hir;
use boron_ir::{Ir, IrLowerer};
use boron_linking::LinkerBuild;
use boron_parser::module::{Module, Modules};
use boron_parser::parser::errors::ModuleNotFound;
use boron_parser::parser::parse;
use boron_resolver::{ResolveVisitor, Resolver};
use boron_source::source_file::SourceFileId;
use boron_thir::{Thir, ThirLowerer};
use std::process::exit;

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
    }
  }

  pub fn check(&mut self) {
    let Some(_entrypoint) = self.file_to_module(self.entry_point) else {
      return;
    };

    self.resolver.build_import_graph(&self.modules);
    if self.run_step(|this| this.resolve_names()) {
      return;
    }
    if self.run_step(|this| this.lower_to_hir()) {
      return;
    }
    if self.run_step(|this| this.validate_comptime()) {
      return;
    }

    if self.run_step(|this| this.typeck()) {
      return;
    }
    if self.run_step(|this| this.expand_builtins()) {
      return;
    }
    if self.run_step(|this| this.lower_to_thir()) {
      return;
    }

    let _ = self.run_step(|this| this.lower_to_ir());
  }

  pub fn build(&mut self) -> Result<()> {
    self.check();
    if self.sess().create_output_dir().is_none() {
      self.sess().dcx().bug("failed to create output directory");
    }

    let Some(ir) = &self.ir else {
      return Ok(());
    };
    match run_codegen(self.sess, ir) {
      Err(err) => {
        self.emit_errors();
        return Err(err);
      }
      _ => {}
    }

    match self.link() {
      Ok(path) => Ok(()),
      Err(err) => Err(err),
    }
  }

  fn link(&mut self) -> Result<PathBuf> {
    let mut build = LinkerBuild::new(self.sess)?;
    build.add_source(self.sess.obj_file());

    build.link(self.sess.config.name.clone())
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

  fn run_step<F>(&mut self, step: F) -> bool
  where
    F: FnOnce(&mut Self),
  {
    step(self);
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
