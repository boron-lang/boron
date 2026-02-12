use log::{debug, warn};
use serde::{Deserialize, Serialize};
use std::{
  fmt::{Display, Formatter},
  path::{Path, PathBuf},
  process::Command,
};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum LinkerError {
  #[error("Linker not found at path: {0}")]
  NotFound(PathBuf),
  #[error("Unsupported linker version: {0}")]
  UnsupportedVersion(String),
  #[error("Failed to detect linker version: {0}")]
  VersionDetectionFailed(String),
  #[error(transparent)]
  IoError(#[from] std::io::Error),
}

#[derive(Debug, Clone, Deserialize, Serialize, PartialEq, Eq, Hash)]
#[serde(rename_all = "lowercase")]
pub enum LinkerKind {
  Ld,
  Lld,
  MsvcLink,
  Mold,
  Gold,
}

impl LinkerKind {
  pub fn is_msvc(&self) -> bool {
    matches!(self, Self::MsvcLink)
  }

  pub fn is_unix_style(&self) -> bool {
    !self.is_msvc()
  }

  pub fn name(&self) -> &str {
    match self {
      Self::Ld => "ld",
      Self::Lld => "lld",
      Self::MsvcLink => "MSVC link",
      Self::Mold => "mold",
      Self::Gold => "gold",
    }
  }

  pub fn executable_names(&self) -> &[&str] {
    match self {
      Self::Ld => &["ld", "ld.bfd"],
      Self::Lld => &["ld.lld", "lld"],
      Self::MsvcLink => &["link.exe", "link"],
      Self::Mold => &["mold"],
      Self::Gold => &["ld.gold", "gold"],
    }
  }
}

impl Display for LinkerKind {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", self.name())
  }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LinkerVersion {
  pub major: u32,
  pub minor: u32,
  pub patch: Option<u32>,
  pub raw: String,
}

impl Display for LinkerVersion {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    if let Some(patch) = self.patch {
      write!(f, "{}.{}.{}", self.major, self.minor, patch)
    } else {
      write!(f, "{}.{}", self.major, self.minor)
    }
  }
}

#[derive(Debug, Clone)]
pub struct Linker {
  path: PathBuf,
  kind: LinkerKind,
  version: Option<LinkerVersion>,
}

impl Display for Linker {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    match &self.version {
      Some(version) => {
        write!(f, "{} {} at {}", self.kind.name(), version, self.path.display())
      }
      None => write!(f, "{} at {}", self.kind.name(), self.path.display()),
    }
  }
}

impl Linker {
  pub fn new(path: PathBuf, kind: LinkerKind) -> Result<Self, LinkerError> {
    if !path.exists() {
      return Err(LinkerError::NotFound(path));
    }

    let mut linker = Self { path, kind, version: None };

    if let Err(e) = linker.detect_version() {
      warn!("Failed to detect linker version: {e}");
    }

    Ok(linker)
  }

  pub fn path(&self) -> &Path {
    &self.path
  }

  pub fn kind(&self) -> &LinkerKind {
    &self.kind
  }

  pub fn is_msvc(&self) -> bool {
    self.kind.is_msvc()
  }

  pub fn version(&self) -> Option<&LinkerVersion> {
    self.version.as_ref()
  }

  fn detect_version(&mut self) -> Result<(), LinkerError> {
    debug!("Detecting version for linker: {}", self.path.display());

    if self.kind == LinkerKind::MsvcLink {
      return Ok(());
    }

    let output = Command::new(&self.path)
      .arg("--version")
      .output()
      .map_err(|e| LinkerError::VersionDetectionFailed(e.to_string()))?;

    let output_str = String::from_utf8_lossy(&output.stdout);
    self.version = self.parse_version(&output_str);

    if let Some(version) = &self.version {
      debug!("Detected linker version: {version}");
    }

    Ok(())
  }

  fn parse_version(&self, output: &str) -> Option<LinkerVersion> {
    for token in output.split_whitespace() {
      if token.chars().next().is_some_and(|c| c.is_ascii_digit()) {
        return Self::parse_version_string(token, output);
      }
    }
    None
  }

  fn parse_version_string(version_str: &str, raw_output: &str) -> Option<LinkerVersion> {
    let clean_version = version_str.split('-').next()?.split('+').next()?;
    let parts: Vec<&str> = clean_version.split('.').collect();

    if parts.len() >= 2 {
      let major = parts[0].parse().ok()?;
      let minor = parts[1].parse().ok()?;
      let patch = parts.get(2).and_then(|p| p.parse().ok());

      Some(LinkerVersion { major, minor, patch, raw: raw_output.to_owned() })
    } else {
      None
    }
  }

  pub fn create_basic_command(&self) -> Command {
    Command::new(&self.path)
  }
}
