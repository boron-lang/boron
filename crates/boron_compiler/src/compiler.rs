use log::{debug, warn};
use serde::{Deserialize, Serialize};
use std::{
  fmt::{Display, Formatter},
  path::{Path, PathBuf},
  process::Command,
};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum CompilerError {
  #[error("Compiler not found at path: {0}")]
  NotFound(PathBuf),
  #[error("Unsupported compiler version: {0}")]
  UnsupportedVersion(String),
  #[error("Failed to detect compiler version: {0}")]
  VersionDetectionFailed(String),
  #[error(transparent)]
  IoError(#[from] std::io::Error),
}

#[derive(Debug, Clone, Deserialize, Serialize, PartialEq, Eq, Hash)]
#[serde(rename_all = "lowercase")]
pub enum CompilerKind {
  Clang,
  Gcc,
}

impl CompilerKind {
  pub fn name(&self) -> &str {
    match self {
      Self::Clang => "clang",
      Self::Gcc => "gcc",
    }
  }

  pub fn executable_names(&self) -> &[&str] {
    match self {
      Self::Clang => &["clang", "clang.exe", "clang-cl", "clang-cl.exe"],
      Self::Gcc => &["gcc", "gcc.exe"],
    }
  }
}

impl Display for CompilerKind {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", self.name())
  }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompilerVersion {
  pub major: u32,
  pub minor: u32,
  pub patch: Option<u32>,
  pub raw: String,
}

impl Display for CompilerVersion {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    if let Some(patch) = self.patch {
      write!(f, "{}.{}.{}", self.major, self.minor, patch)
    } else {
      write!(f, "{}.{}", self.major, self.minor)
    }
  }
}

#[derive(Debug, Clone)]
pub struct Compiler {
  path: PathBuf,
  kind: CompilerKind,
  version: Option<CompilerVersion>,
}

impl Display for Compiler {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    match &self.version {
      Some(version) => {
        write!(f, "{} {} at {}", self.kind.name(), version, self.path.display())
      }
      None => write!(f, "{} at {}", self.kind.name(), self.path.display()),
    }
  }
}

impl Compiler {
  pub fn new(path: PathBuf, kind: CompilerKind) -> Result<Self, CompilerError> {
    if !path.exists() {
      return Err(CompilerError::NotFound(path));
    }

    let mut compiler = Self { path, kind, version: None };

    if let Err(e) = compiler.detect_version() {
      warn!("Failed to detect compiler version: {e}");
    }

    Ok(compiler)
  }

  pub fn path(&self) -> &Path {
    &self.path
  }

  pub fn kind(&self) -> &CompilerKind {
    &self.kind
  }

  pub fn version(&self) -> Option<&CompilerVersion> {
    self.version.as_ref()
  }

  fn detect_version(&mut self) -> Result<(), CompilerError> {
    debug!("Detecting version for compiler: {}", self.path.display());

    let output = Command::new(&self.path)
      .arg("--version")
      .output()
      .map_err(|e| CompilerError::VersionDetectionFailed(e.to_string()))?;

    let output_str = String::from_utf8_lossy(&output.stdout);
    self.version = self.parse_version(&output_str);

    if let Some(version) = &self.version {
      debug!("Detected compiler version: {version}");
    }

    Ok(())
  }

  fn parse_version(&self, output: &str) -> Option<CompilerVersion> {
    for token in output.split_whitespace() {
      if token.chars().next().is_some_and(|c| c.is_ascii_digit()) {
        return Self::parse_version_string(token, output);
      }
    }
    None
  }

  fn parse_version_string(
    version_str: &str,
    raw_output: &str,
  ) -> Option<CompilerVersion> {
    let clean_version = version_str.split('-').next()?.split('+').next()?;
    let parts: Vec<&str> = clean_version.split('.').collect();

    if parts.len() >= 2 {
      let major = parts[0].parse().ok()?;
      let minor = parts[1].parse().ok()?;
      let patch = parts.get(2).and_then(|p| p.parse().ok());

      Some(CompilerVersion { major, minor, patch, raw: raw_output.to_owned() })
    } else {
      None
    }
  }

  pub fn create_basic_command(&self) -> Command {
    Command::new(&self.path)
  }
}
