use super::{CompilerArgStyle, CompilerTool, resolve_executable};
use crate::compiler::CompilerKind;
use anyhow::Result;
use boron_session::prelude::Session;
use std::path::PathBuf;

pub struct GccCompiler;

impl CompilerTool for GccCompiler {
  fn kind(&self) -> CompilerKind {
    CompilerKind::Gcc
  }

  fn resolve_path(&self, _sess: &Session) -> Result<PathBuf> {
    resolve_executable(&["gcc", "gcc.exe"])
  }

  fn arg_style(&self, _sess: &Session) -> CompilerArgStyle {
    CompilerArgStyle::unix()
  }
}
