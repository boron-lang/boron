use boron_cli::logger::setup_logger;
use boron_cli::Cli;
use boron_core::prelude::*;
use clap::Parser as _;
use std::io::{stderr, Cursor, Write as _};
use std::sync::Arc;
use std::time::Instant;

pub fn try_cli() -> Result<()> {
  let instant = Instant::now();
  let cli = Cli::parse();
  setup_logger(cli.verbose, cli.no_color);
  let check_only = cli.check_only;
  let project_config = ProjectConfig::try_from(cli)?;

  let output = Arc::new(Mutex::new(Cursor::new(vec![])));
  compiler_entrypoint(&project_config, output.clone(), false, check_only)?;
  stderr().write_all(output.lock().get_ref())?;

  info!("finished in {:.2?}", instant.elapsed());
  Ok(())
}
