use super::{LinkerArgStyle, LinkerTool, resolve_executable};
use crate::linker::LinkerKind;
use anyhow::Result;
use boron_session::prelude::Session;
use std::path::PathBuf;

pub struct GoldLinker;

impl LinkerTool for GoldLinker {
  fn kind(&self) -> LinkerKind {
    LinkerKind::Gold
  }

  fn resolve_path(&self, _sess: &Session) -> Result<PathBuf> {
    resolve_executable(&["ld.gold", "gold"])
  }

  fn arg_style(&self, _sess: &Session) -> LinkerArgStyle {
    LinkerArgStyle::unix()
  }
}
