use crate::panic::setup_panic_handler;
use anyhow::Result;
use boron_cli::prelude::{
  PROJECT_FILE, build_project_config, load_project_toml, setup_logger,
};
use boron_cli::{Cli, CliCommand};
use boron_core::prelude::{
  CompilationMode, DiagnosticWriter, Session, compiler_entrypoint,
};
use boron_lib::container::read_container_file;
use clap::Parser as _;
use std::path::Path;
use std::process::exit;
use yansi::Paint as _;

mod panic;

fn main() {
  if let Err(e) = run() {
    eprintln!("{} {e:?}", "error".bright_red().bold());
    exit(1);
  }
}

fn run() -> Result<()> {
  let cli = Cli::parse();

  if let Some(command) = &cli.command {
    return run_command(command);
  }

  let toml = load_project_toml(Path::new(PROJECT_FILE))?;
  let project_config = build_project_config(cli, toml)?;
  let session =
    Session::new(project_config, DiagnosticWriter::stderr(), CompilationMode::Normal);

  setup_panic_handler(&session);
  setup_logger(session.config.verbose, !session.config.color);

  if let Err(e) = compiler_entrypoint(&session) {
    Err(e)
  } else {
    if session.config.timings {
      session.print_timings();
    }

    Ok(())
  }
}

fn run_command(command: &CliCommand) -> Result<()> {
  match command {
    CliCommand::InspectBlib { file } => {
      let container = read_container_file(file)?;

      println!("Metadata:\n{:#?}", container.metadata);
      println!("Files: {}", container.files.len());

      for file in container.files {
        println!("\n{} ({} bytes)", file.name.bold().dim(), file.data.len());
        println!("{}", String::from_utf8_lossy(&file.data));
      }

      Ok(())
    }
  }
}
