use crate::linker::LinkerKind;
use anyhow::{Result, anyhow};
use boron_session::prelude::Session;
use boron_target::target::Os;
use std::ffi::OsStr;
use std::path::{Path, PathBuf};
use std::process::Command;

mod gold;
mod ld;
mod lld;
mod mold;
mod msvc;

pub use gold::GoldLinker;
pub use ld::LdLinker;
pub use lld::LldLinker;
pub use mold::MoldLinker;
pub use msvc::MsvcLinker;

pub trait LinkerTool {
  fn kind(&self) -> LinkerKind;
  fn resolve_path(&self, sess: &Session) -> Result<PathBuf>;
  fn arg_style(&self, sess: &Session) -> LinkerArgStyle;
  fn is_available_on(&self, _sess: &Session) -> bool {
    true
  }
}

pub fn linker_tool(kind: LinkerKind) -> Box<dyn LinkerTool> {
  match kind {
    LinkerKind::Ld => Box::new(LdLinker),
    LinkerKind::Lld => Box::new(LldLinker),
    LinkerKind::MsvcLink => Box::new(MsvcLinker),
    LinkerKind::Mold => Box::new(MoldLinker),
    LinkerKind::Gold => Box::new(GoldLinker),
  }
}

pub fn resolve_executable(names: &[&str]) -> Result<PathBuf> {
  for name in names {
    if let Ok(path) = which::which(name) {
      return Ok(path);
    }
  }
  Err(anyhow!("linker not found in PATH: {names:?}"))
}

#[derive(Copy, Clone)]
pub struct LinkerArgStyle {
  lib_dir_prefix: &'static str,
  lib_prefix: &'static str,
  output_prefix: &'static str,
  shared_flag: &'static str,
  pub no_logo_flag: &'static str,
  output_is_prefix: bool,
  lib_dir_is_prefix: bool,
  lib_is_prefix: bool,
}

impl LinkerArgStyle {
  pub fn msvc() -> Self {
    Self {
      lib_dir_prefix: "/LIBPATH:",
      lib_prefix: "/DEFAULTLIB:",
      output_prefix: "/OUT:",
      shared_flag: "/DLL",
      no_logo_flag: "/NOLOGO",
      output_is_prefix: true,
      lib_dir_is_prefix: true,
      lib_is_prefix: true,
    }
  }

  pub fn unix() -> Self {
    Self {
      lib_dir_prefix: "-L",
      lib_prefix: "-l",
      output_prefix: "-o",
      shared_flag: "-shared",
      no_logo_flag: "-w",
      output_is_prefix: false,
      lib_dir_is_prefix: false,
      lib_is_prefix: false,
    }
  }

  pub fn push_library_dir(&self, command: &mut Command, dir: &Path) {
    if self.lib_dir_is_prefix {
      command.arg(format!("{}{}", self.lib_dir_prefix, dir.display()));
    } else {
      command.arg(self.lib_dir_prefix);
      command.arg(dir.as_os_str());
    }
  }

  pub fn push_library(&self, command: &mut Command, lib: &OsStr) {
    if self.lib_is_prefix {
      command.arg(format!("{}{}", self.lib_prefix, lib.to_string_lossy()));
    } else {
      command.arg(self.lib_prefix);
      command.arg(lib);
    }
  }

  pub fn push_output(&self, command: &mut Command, output_path: &Path) {
    if self.output_is_prefix {
      command.arg(format!("{}{}", self.output_prefix, output_path.display()));
    } else {
      command.arg(self.output_prefix);
      command.arg(output_path);
    }
  }

  pub fn push_shared_library(&self, command: &mut Command) {
    command.arg(self.shared_flag);
  }
}

pub fn is_windows(sess: &Session) -> bool {
  sess.target().os == Os::Windows
}
