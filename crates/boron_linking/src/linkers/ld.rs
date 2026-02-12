use super::{LinkerArgStyle, LinkerTool, resolve_executable};
use crate::linker::LinkerKind;
use anyhow::Result;
use boron_session::prelude::Session;
use std::path::PathBuf;

pub struct LdLinker;

impl LinkerTool for LdLinker {
  fn kind(&self) -> LinkerKind {
    LinkerKind::Ld
  }

  fn resolve_path(&self, _sess: &Session) -> Result<PathBuf> {
    resolve_executable(&["ld", "ld.bfd"])
  }

  fn arg_style(&self, _sess: &Session) -> LinkerArgStyle {
    LinkerArgStyle::unix()
  }
}
