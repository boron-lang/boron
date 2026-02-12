use super::{LinkerArgStyle, LinkerTool, is_windows, resolve_executable};
use crate::linker::LinkerKind;
use anyhow::Result;
use boron_session::prelude::Session;
use std::path::PathBuf;

pub struct LldLinker;

impl LinkerTool for LldLinker {
  fn kind(&self) -> LinkerKind {
    LinkerKind::Lld
  }

  fn resolve_path(&self, sess: &Session) -> Result<PathBuf> {
    if is_windows(sess) {
      resolve_executable(&["lld-link", "lld-link.exe", "lld"])
    } else {
      resolve_executable(&["ld.lld", "lld"])
    }
  }

  fn arg_style(&self, sess: &Session) -> LinkerArgStyle {
    if is_windows(sess) { LinkerArgStyle::msvc() } else { LinkerArgStyle::unix() }
  }
}
