use crate::prelude::{LibType, PackageType};
use boron_diagnostics::prelude::DiagnosticOutputType;
use boron_source::new_id;
use boron_target::target::Compiler;
use parking_lot::RwLock;
use serde::{Deserialize, Serialize};
use std::{path::PathBuf, sync::Arc};

new_id!(DepId);

#[derive(Debug, Clone, Serialize, Deserialize)]
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
}

impl Dependency {
  pub fn new(name: String, entrypoint: PathBuf, root: PathBuf) -> Self {
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

#[derive(Debug, Clone, Default)]
pub struct Dependencies(Arc<RwLock<Vec<Dependency>>>);

impl Dependencies {
  pub fn new(dependencies: Vec<Dependency>) -> Self {
    Self(Arc::new(RwLock::new(dependencies)))
  }

  fn read<R>(&self, reader: impl FnOnce(&Vec<Dependency>) -> R) -> R {
    reader(&self.0.read())
  }

  fn write<R>(&self, writer: impl FnOnce(&mut Vec<Dependency>) -> R) -> R {
    writer(&mut self.0.write())
  }

  pub fn get(&self, name: &str) -> Option<Dependency> {
    self.read(|deps| deps.iter().find(|dep| dep.name == name).cloned())
  }

  pub fn add(&self, dependency: Dependency) {
    self.write(|deps| deps.push(dependency));
  }

  pub fn all(&self) -> Vec<Dependency> {
    self.read(|deps| deps.clone())
  }

  pub fn contains(&self, dependency: &Dependency) -> bool {
    self.read(|deps| deps.iter().any(|dep| dep.name == dependency.name))
  }
}
