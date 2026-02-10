use crate::panic::setup_panic_handler;
use anyhow::Result;
use boron_cli::prelude::setup_logger;
use boron_cli::Cli;
use boron_core::prelude::*;
use clap::Parser;
use std::process::exit;
use std::time::Instant;

mod panic;

fn main() -> Result<()> {
  let cli = Cli::parse();
  let project_config = ProjectConfig::try_from(cli)?;
  let session =
    Session::new(project_config, DiagnosticWriter::stderr(), CompilationMode::Normal);

  setup_panic_handler(&session);
  setup_logger(session.config.verbose, !session.config.color);

  let instant = Instant::now();
  if let Err(e) = compiler_entrypoint(&session) {
    error!("{e:?}");
    exit(1);
  } else {
    info!("finished in {:.2?}", instant.elapsed());
  }

  Ok(())
}
