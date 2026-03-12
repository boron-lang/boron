use anyhow::bail;
use boron_diagnostics::prelude::DiagnosticOutputType;
use boron_target::target::Compiler;
use parking_lot::RwLock;
use std::{path::PathBuf, str::FromStr, sync::Arc};

use crate::prelude::{LibType, PackageType};

#[derive(Debug, Clone)]
pub struct Dependency {
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

impl FromStr for Dependency {
  type Err = anyhow::Error;

  // Format: name:root=entrypoint[:dep1,dep2,...]
  fn from_str(s: &str) -> anyhow::Result<Self, Self::Err> {
    let Some((name, rest)) = s.split_once(':') else {
      bail!(
        "Invalid dependency format. Expected 'name:root=entrypoint[:dep1,dep2]', got '{s}'"
      )
    };

    let Some((root, path_and_deps)) = rest.split_once('=') else {
      bail!(
        "Invalid dependency format. Expected 'name:root=entrypoint[:dep1,dep2]', got '{s}'"
      )
    };

    let (entrypoint_str, depends_on) = match path_and_deps.split_once(':') {
      Some((ep, deps_str)) => {
        let depends_on = deps_str
          .split(',')
          .map(str::trim)
          .filter(|s| !s.is_empty())
          .map(str::to_owned)
          .collect();
        (ep, depends_on)
      }
      None => (path_and_deps, Vec::new()),
    };

    Ok(Self {
      name: name.to_owned(),
      root: PathBuf::from(root),
      entrypoint: PathBuf::from(entrypoint_str),
      depends_on,
      package_type: None,
      output: None,
      lib_type: None,
      compiler: None,
      diagnostic_output_type: None,
    })
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
