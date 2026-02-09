use boron_cli::Cli;
use boron_cli::logger::setup_logger;
use boron_core::prelude::*;
use clap::Parser as _;
use std::time::Instant;

pub fn try_cli() -> Result<()> {
  let instant = Instant::now();
  let cli = Cli::parse();
  setup_logger(cli.verbose, cli.no_color);
  let check_only = cli.check_only;
  let project_config = ProjectConfig::try_from(cli)?;

  compiler_entrypoint(&project_config, DiagnosticWriter::stderr(), false, check_only)?;
  info!("finished in {:.2?}", instant.elapsed());
  Ok(())
}
