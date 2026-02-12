use crate::linker::{Linker, LinkerKind};
use crate::linkers::linker_tool;
use anyhow::{Context as _, Result, anyhow};
use boron_session::prelude::Session;
use boron_target::target::Os;
use log::debug;
use std::path::PathBuf;

pub fn detect_linker(sess: &Session) -> Result<Linker> {
  debug!("Detecting linker");

  let linker_kinds = linker_kinds_for_os(sess.target().os);

  if let Some(kind) = linker_kinds.into_iter().next() {
    return match resolve_from_kind(kind.clone(), sess) {
      Ok(linker) => {
        debug!("Successfully detected {} linker", linker.kind().name());
        Ok(linker)
      }
      Err(e) => Err(anyhow!("Failed to resolve {kind} linker: {e}")),
    };
  }

  Err(anyhow!("No suitable linker found"))
}

fn linker_kinds_for_os(os: Os) -> Vec<LinkerKind> {
  match os {
    Os::Windows => vec![
      LinkerKind::MsvcLink,
      LinkerKind::Lld,
      LinkerKind::Mold,
      LinkerKind::Gold,
      LinkerKind::Ld,
    ],
    Os::Linux | Os::MacOs => {
      vec![LinkerKind::Mold, LinkerKind::Lld, LinkerKind::Gold, LinkerKind::Ld]
    }
  }
}

pub fn resolve_from_kind(kind: LinkerKind, sess: &Session) -> Result<Linker> {
  debug!("Resolving linker of kind: {kind:?}");

  let tool = linker_tool(kind.clone());
  if !tool.is_available_on(sess) {
    return Err(anyhow!("{kind} linker is not available on this target"));
  }

  let path: PathBuf = tool.resolve_path(sess)?;
  Linker::new(path, kind).context("Failed to initialize linker")
}

pub fn detect_all_linkers(sess: &Session) -> Vec<Linker> {
  let mut linkers = Vec::new();

  let linker_kinds = linker_kinds_for_os(sess.target().os);

  for kind in linker_kinds {
    if let Ok(linker) = resolve_from_kind(kind, sess) {
      linkers.push(linker);
    }
  }

  debug!("Detected {} linkers total", linkers.len());
  linkers
}

pub fn get_best_linker(sess: &Session) -> Result<Linker> {
  detect_linker(sess).context("No suitable linker found on this system")
}
