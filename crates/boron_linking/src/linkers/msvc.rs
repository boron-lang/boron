use super::{LinkerArgStyle, LinkerTool, is_windows};
use crate::linker::LinkerKind;
use anyhow::{Result, anyhow};
use boron_session::prelude::Session;
use find_msvc_tools::find_tool;
use std::path::PathBuf;

pub struct MsvcLinker;

impl LinkerTool for MsvcLinker {
  fn kind(&self) -> LinkerKind {
    LinkerKind::MsvcLink
  }

  fn resolve_path(&self, sess: &Session) -> Result<PathBuf> {
    let tool = find_tool(&sess.target().arch.to_string(), "link.exe");
    let Some(tool) = tool else { return Err(anyhow!("Couldn't find {}", self.kind())) };

    Ok(tool.path().to_path_buf())
  }

  fn arg_style(&self, _sess: &Session) -> LinkerArgStyle {
    LinkerArgStyle::msvc()
  }

  fn is_available_on(&self, sess: &Session) -> bool {
    is_windows(sess)
  }
}
