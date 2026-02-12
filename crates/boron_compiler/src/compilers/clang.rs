use super::{resolve_executable, CompilerArgStyle, CompilerTool};
use crate::compiler::CompilerKind;
use anyhow::Result;
use boron_session::prelude::Session;
use std::path::PathBuf;

pub struct ClangCompiler;

impl CompilerTool for ClangCompiler {
  fn kind(&self) -> CompilerKind {
    CompilerKind::Clang
  }

  fn resolve_path(&self, _sess: &Session) -> Result<PathBuf> {
    resolve_executable(&["clang", "clang.exe"])
  }

  fn arg_style(&self, _sess: &Session) -> CompilerArgStyle {
    let mut args = CompilerArgStyle::unix();

    args.add_arg(format!("--target={}", _sess.target().triple()));
    args
  }
}
