use crate::module_graph::ModuleGraph;
use crate::prelude::{create_dir_all, PackageType};
use crate::project_config::ProjectConfig;
use boron_diagnostics::{DiagnosticCtx, DiagnosticWriter};
use boron_source::prelude::Sources;
use boron_target::target::{Compiler, Target};
use parking_lot::RwLock;
use std::path::PathBuf;
use std::sync::Arc;
use std::time::Duration;
use yansi::Paint;

pub struct Session {
  pub config: ProjectConfig,
  dcx: DiagnosticCtx,
  module_graph: ModuleGraph,
  target: Target,
  compiler: Option<Compiler>,
  compilation_mode: CompilationMode,
  sources: Arc<Sources>,
  timings: RwLock<Vec<(String, Duration)>>,
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
    let compiler = config.compiler;

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
      compiler,
      compilation_mode,
      sources,
      timings: RwLock::new(Vec::new()),
    }
  }

  pub fn create_output_dir(&self) -> Option<()> {
    create_dir_all(&self.config.output).ok()
  }

  pub fn target(&self) -> &Target {
    &self.target
  }

  pub fn compiler(&self) -> Option<Compiler> {
    self.compiler
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

  pub fn store_timing(&self, step: &str, duration: Duration) {
    let mut write = self.timings.write();
    write.push((step.to_string(), duration));
  }

  pub fn print_timings(&self) {
    let timings = self.timings.read();

    let total: Duration = timings.iter().map(|(_, d)| *d).sum();
    let max_len = timings.iter().map(|(step, _)| step.len()).max().unwrap_or(0);

    println!("\n{}", "Timing Summary".bold().underline());
    println!("{}", "─".repeat(max_len + 20));

    for (step, duration) in timings.iter() {
      let percentage = duration.as_secs_f64() / total.as_secs_f64() * 100.0;

      println!(
        "{:<width$}  {:>8.2?}  {:>5.1}%",
        step,
        duration,
        percentage,
        width = max_len
      );
    }

    println!("{}", "─".repeat(max_len + 20));
    println!("{:<width$}  {:>8.2?}", "Total".bold(), total.bold(), width = max_len);
  }

  pub fn is_lib(&self) -> bool {
    self.config.package_type == PackageType::Library
  }

  pub fn is_binary(&self) -> bool {
    self.config.package_type == PackageType::Binary
  }
}
