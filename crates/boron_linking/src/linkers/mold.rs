use super::{LinkerArgStyle, LinkerTool, resolve_executable};
use crate::linker::LinkerKind;
use anyhow::Result;
use boron_session::prelude::Session;
use std::path::PathBuf;

pub struct MoldLinker;

impl LinkerTool for MoldLinker {
  fn kind(&self) -> LinkerKind {
    LinkerKind::Mold
  }

  fn resolve_path(&self, _sess: &Session) -> Result<PathBuf> {
    resolve_executable(&["mold"])
  }

  fn arg_style(&self, _sess: &Session) -> LinkerArgStyle {
    LinkerArgStyle::unix()
  }
}
