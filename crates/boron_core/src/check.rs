use crate::prelude::{info, CompilationUnit, FILE_EXTENSION};
use anyhow::bail;
use anyhow::Result;
use boron_session::prelude::Session;
use yansi::Paint;

pub fn compiler_entrypoint(session: &Session) -> Result<()> {
  let file = &session.config.entrypoint;
  if let Some(ext) = file.extension() {
    if ext != FILE_EXTENSION {
      bail!(
        "Found an entry point with invalid extension: {}. It must be {}",
        file.display(),
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
    info!("{} without any problems", "Checked".underline().bold())
  } else {
    unit.build()?;
  }

  Ok(())
}
