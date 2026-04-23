use crate::errors::{
  MainNoGenerics, MainNoParams, MainRetNotAUnit, ModuleNotFound, NoMainFunction,
};
use crate::prelude::*;
use boron_analysis::results::BuiltInResults;
use boron_analysis::validator::validate_comptime;
use boron_analysis::{expand_builtins, typeck_hir};
use boron_codegen::run_codegen;
use boron_compiler::CompilerBuild;
use boron_context::BCtx;
use boron_hir::lower::lower_to_hir;
use boron_ir::IrLowerer;
use boron_parser::{Lexer, parser::parse};
use boron_resolver::{DefId, ResolveVisitor};
use boron_source::StablePackageId;
use boron_source::source_file::SourceFileId;
use boron_thir::ThirLowerer;
use boron_types::ast::module::Module;
use boron_types::hir::TyKind;
use std::process::exit;
use std::time::Instant;

pub struct CompilationUnit<'ctx> {
  pub entry_point: SourceFileId,
  pub sess: &'ctx Session,
  pub ctx: &'ctx BCtx<'ctx>,
  pub builtin_results: Option<BuiltInResults>,
  pub main_function: Option<DefId>,
  pub stable_id: StablePackageId,
}

macro_rules! steps {
    ($this:expr; $($name:expr => $action:expr),* $(,)?) => {
        $(if $this.run_step($name, $action) { return; })*
    };
}

impl<'ctx> CompilationUnit<'ctx> {
  pub fn new(
    entry_point: SourceFileId,
    sess: &'ctx Session,
    ctx: &'ctx BCtx<'ctx>,
  ) -> Self {
    Self {
      entry_point,
      sess,
      ctx,
      builtin_results: None,
      main_function: None,
      stable_id: StablePackageId::new(
        sess.config.name.clone(),
        sess.config.version.clone(),
        sess.config.package_type == PackageType::Binary,
      ),
    }
  }

  pub fn check(&mut self) {
    let Some(_entrypoint) = self.file_to_module(self.entry_point) else {
      return;
    };
    self.ctx.set_current_pkg_id(self.ctx.pkg_id(self.stable_id));

    steps!(self;
      "Import graph" => |this| this.build_import_graph(),
      "Name resolution" => |this| {
        if !ResolveVisitor::resolve_modules(this.entry_point, this.ctx, self.sess) {
        }
      },
      "HIR lowering" => |this| this.lower_to_hir(),
      "Comptime Validation" => |this|  this.validate_comptime(),
      "Type inference and checking" => |this| this.typeck(),
      "Built-in expanding" => |this| this.expand_builtins(),
      "THIR lowering" => |this| this.lower_to_thir(),
      "Main function validation" => |this| this.find_main_function(),
      "IR lowering" => |this| this.lower_to_ir()
    );
  }

  fn build_import_graph(&self) {
    self.ctx.resolver().build_import_graph(self.ctx.modules());
  }

  fn find_main_function(&mut self) {
    if self.sess.is_lib() {
      return;
    }

    let ctx = self.ctx;
    let main = ctx.hir().functions.iter().find(|func| func.name.text() == "main");

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

      if !matches!(main.return_type.kind, TyKind::Unit) {
        self.sess.dcx().emit(MainRetNotAUnit { span: main.return_type.span });
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

    let ir = IrLowerer::new(self.sess, self.ctx).lower();

    let start = Instant::now();
    if let Err(err) = run_codegen(self.sess, &ir, self.main_function) {
      self.emit_errors();
      return Err(err);
    }
    self.sess.store_timing("LLVM codegen", start.elapsed());

    let linking_start = Instant::now();
    match self.link() {
      Ok(_path) => {
        self.sess.store_timing("Linking", linking_start.elapsed());
        Ok(())
      }
      Err(err) => Err(err),
    }
  }

  fn link(&self) -> Result<PathBuf> {
    let mut build = CompilerBuild::new(self.sess)?;
    build.add_source(self.sess.obj_file());

    build.compile(self.sess.config.name.clone())
  }

  fn lower_to_thir(&mut self) {
    let Some(builtin_results) = &self.builtin_results else {
      return;
    };

    ThirLowerer::new(self.ctx, self.sess.dcx(), builtin_results).lower();
  }

  fn lower_to_ir(&mut self) {}

  fn expand_builtins(&mut self) {
    self.builtin_results = Some(expand_builtins(self.sess, self.ctx));
  }

  fn validate_comptime(&self) {
    let dcx = self.sess.dcx();

    validate_comptime(dcx, self.ctx);
  }

  fn typeck(&mut self) {
    typeck_hir(self.sess, self.ctx);
  }

  fn lower_to_hir(&mut self) {
    let dcx = self.sess.dcx();
    lower_to_hir(self.ctx, dcx);
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
    self.ctx.modules().add(module);

    let files_to_process: Vec<_> = {
      let this = self.ctx.modules().get_unchecked(id);
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
