use crate::library::BLibMetadata;
use crate::prelude::{LibType, PackageType};
use boron_diagnostics::prelude::DiagnosticOutputType;
use boron_source::new_id;
use boron_target::target::Compiler;
use serde::{Deserialize, Serialize};
use std::path::PathBuf;

new_id!(DepId);

#[derive(Debug, Serialize, Deserialize)]
pub struct Dependency {
  pub id: DepId,
  pub name: String,
  pub root: PathBuf,
  pub entrypoint: PathBuf,
  pub depends_on: Vec<String>,
  pub package_type: Option<PackageType>,
  pub output: Option<PathBuf>,
  pub lib_type: Option<LibType>,
  pub compiler: Option<Compiler>,
  pub diagnostic_output_type: Option<DiagnosticOutputType>,
  pub blib: BLibMetadata,
}

impl Dependency {
  pub fn new(
    name: String,
    blib: BLibMetadata,
    entrypoint: PathBuf,
    root: PathBuf,
  ) -> Self {
    Self {
      id: DepId::new(),
      name,
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
