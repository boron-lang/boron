use crate::prelude::{CompilationUnit, FILE_EXTENSION};
use anyhow::bail;
use anyhow::Result;
use boron_diagnostics::DiagnosticWriter;
use boron_source::prelude::Sources;
use boron_utils::prelude::{info, CompilationMode, ProjectConfig, Session};
use std::sync::Arc;

pub fn compiler_entrypoint(session: &Session) -> Result<()> {
  let file = &session.config.entrypoint;
  info!("checking entrypoint: {} with {} mode", file.display(), session.config().mode);

  if let Some(ext) = file.extension() {
    if ext != FILE_EXTENSION {
      bail!(
        "Found an entry point with invalid extension: {}. It must be {}",
        file.display().to_string(),
        FILE_EXTENSION
      );
    }
  }

  let file = session.root().join(file);
  let contents = fs_err::read_to_string(file.clone())?;

  let file_id = session.sources().add(contents, file.clone());
  let mut unit = CompilationUnit::new(file_id, session);

  if session.config().check_only {
    unit.check();
  } else {
    unit.build()?;
  }

  Ok(())
}
