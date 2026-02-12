use crate::compiler::{Compiler, CompilerKind};
use crate::compilers::compiler_tool;
use anyhow::{anyhow, Context as _, Result};
use boron_session::prelude::Session;
use boron_target::target::Os;
use log::debug;
use std::path::PathBuf;

pub fn resolve_from_kind(kind: CompilerKind, sess: &Session) -> Result<Compiler> {
  debug!("Resolving compiler of kind: {kind:?}");

  let tool = compiler_tool(kind.clone());
  if !tool.is_available_on(sess) {
    return Err(anyhow!("{kind} compiler is not available on this target"));
  }

  let path: PathBuf = tool.resolve_path(sess)?;
  Compiler::new(path, kind).context("Failed to initialize compiler")
}
