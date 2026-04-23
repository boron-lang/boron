use crate::library::BLibMetadata;
use crate::prelude::{LibType, PackageType};
use boron_diagnostics::prelude::DiagnosticOutputType;
use boron_target::target::Compiler;
pub use boron_types::DepId;
use serde::{Deserialize, Serialize};
use std::path::PathBuf;

#[derive(Debug, Serialize, Deserialize)]
pub struct Dependency {
  pub id: DepId,
  pub name: String,
  pub version: String,
  pub root: PathBuf,
  pub entrypoint: PathBuf,
  pub depends_on: Vec<String>,
  pub package_type: Option<PackageType>,
  pub output: Option<PathBuf>,
  pub lib_type: Option<LibType>,
  pub compiler: Option<Compiler>,
  pub diagnostic_output_type: Option<DiagnosticOutputType>,
  pub blib: Option<BLibMetadata>,
}

impl Dependency {
  pub fn new(
    name: String,
    version: String,
    blib: Option<BLibMetadata>,
    entrypoint: PathBuf,
    root: PathBuf,
  ) -> Self {
    Self {
      id: DepId::new(),
      name,
      version,
      root,
      entrypoint,
      depends_on: Vec::new(),
      package_type: None,
      output: None,
      lib_type: None,
      compiler: None,
      diagnostic_output_type: None,
      blib,
    }
  }

  pub fn name(&self) -> &str {
    &self.name
  }

  pub fn entrypoint(&self) -> &PathBuf {
    &self.entrypoint
  }

  pub fn root(&self) -> &PathBuf {
    &self.root
  }

  pub fn depends_on(&self) -> &[String] {
    &self.depends_on
  }
}
