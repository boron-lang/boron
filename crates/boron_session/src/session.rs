use crate::module_graph::ModuleGraph;
use crate::prelude::create_dir_all;
use crate::project_config::ProjectConfig;
use boron_diagnostics::{DiagnosticCtx, DiagnosticWriter};
use boron_source::prelude::Sources;
use boron_target::target::{Linker, Target};
use std::path::PathBuf;
use std::sync::Arc;

pub struct Session {
  pub config: ProjectConfig,
  dcx: DiagnosticCtx,
  module_graph: ModuleGraph,
  target: Target,
  linker: Option<Linker>,
  compilation_mode: CompilationMode,
  sources: Arc<Sources>,
}

#[derive(Debug, Clone, PartialEq, Eq, Copy, Default)]
pub enum CompilationMode {
  #[default]
  Normal,
  /// This mode is active when running the compiler test suite
  TestRunner,
}

impl Session {
  pub fn new(
    config: ProjectConfig,
    writer: DiagnosticWriter,
    compilation_mode: CompilationMode,
  ) -> Self {
    let sources = Arc::new(Sources::with_root(config.root.clone()));
    let linker = config.linker;

    Self {
      dcx: DiagnosticCtx::new(
        Arc::clone(&sources),
        config.color,
        &config.diagnostic_output_type,
        writer,
      ),
      config,
      module_graph: ModuleGraph::new(),
      target: Target::host(),
      linker,
      compilation_mode,
      sources,
    }
  }

  pub fn create_output_dir(&self) -> Option<()> {
    create_dir_all(&self.config.output).ok()
  }

  pub fn target(&self) -> &Target {
    &self.target
  }

  pub fn linker(&self) -> Option<Linker> {
    self.linker
  }

  pub fn config(&self) -> &ProjectConfig {
    &self.config
  }

  pub fn is_test(&self) -> bool {
    self.compilation_mode == CompilationMode::TestRunner
  }

  pub fn dcx(&self) -> &DiagnosticCtx {
    &self.dcx
  }

  pub fn root(&self) -> PathBuf {
    self.config.root.clone()
  }

  pub fn sources(&self) -> &Arc<Sources> {
    &self.sources
  }

  pub fn graph(&self) -> &ModuleGraph {
    &self.module_graph
  }

  pub fn obj_dir(&self) -> PathBuf {
    self.config.output.join("obj")
  }

  pub fn create_obj_dir(&self) {
    if let Err(err) = fs_err::create_dir_all(self.obj_dir()) {
      self.dcx.error(format!("Couldn't create obj director: {err}"));
    }
  }

  /// Returns path to obj file of current package
  pub fn obj_file(&self) -> PathBuf {
    self.obj_dir().join(format!(
      "{}{}",
      self.config.name,
      self.target().obj_file_suffix()
    ))
  }
}
