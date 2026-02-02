use crate::prelude::{Colorize as _, CompilationUnit, FILE_EXTENSION};
use anyhow::bail;
use anyhow::Result;
use boron_diagnostics::DiagnosticWriter;
use boron_source::prelude::Sources;
use boron_utils::context::Context;
use boron_utils::prelude::{info, ProjectConfig, Session};
use std::sync::Arc;

pub fn compiler_entrypoint(
  config: &ProjectConfig,
  writer: DiagnosticWriter,
  is_test: bool,
  check_only: bool,
) -> Result<Session> {
  let file = &config.entrypoint;
  info!("checking entrypoint: {} with {} mode", file.display(), config.mode);

  if let Some(ext) = file.extension() {
    if ext != FILE_EXTENSION {
      bail!(
        "Found an entry point with invalid extension: {}. It must be {}",
        file.display().to_string().dimmed(),
        FILE_EXTENSION.dimmed()
      );
    }
  }
  let sources = Arc::new(Sources::new());

  let sess = Session::new(config.clone(), &sources, writer, is_test);
  let context = &mut Context::new(&sess, Arc::clone(&sources));

  let file = sess.config().root.join(file);
  let contents = fs_err::read_to_string(file.clone())?;

  let file_id = sources.add(contents, file.clone());
  let mut unit = CompilationUnit::new(file_id, context);

  if check_only {
    unit.check();
  } else {
    unit.build();
  }

  Ok(sess)
}
