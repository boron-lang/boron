use crate::panic::setup_panic_handler;
use anyhow::Result;
use boron_cli::prelude::CliBuild;
use boron_cli::prelude::{build_project_config, setup_logger};
use boron_cli::{Cli, CliCommand};
use boron_core::prelude::{
  CompilationMode, DiagnosticWriter, Session, compiler_entrypoint, debug,
};
use boron_lib::container::read_container_file;
use clap::Parser as _;
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

  match cli.command {
    CliCommand::Build(build_args) => run_build(build_args),
    CliCommand::InspectBlib { file } => run_inspect_blib(file),
  }
}

fn run_build(build_args: CliBuild) -> Result<()> {
  let (project_config, dependencies) = build_project_config(build_args)?;
  let mut session =
    Session::new(project_config, DiagnosticWriter::stderr(), CompilationMode::Normal);

  setup_panic_handler(&session);
  setup_logger(session.config.verbose, !session.config.color);
  debug!(?session);

  compiler_entrypoint(&mut session, dependencies)?;

  if session.config.timings {
    session.print_timings();
  }

  Ok(())
}

fn run_inspect_blib(file: std::path::PathBuf) -> Result<()> {
  let container = read_container_file(file)?;

  println!("Metadata:\n{:#?}", container.metadata);
  println!("Files: {}", container.files.len());

  for file in container.files {
    println!("\n{} ({} bytes)", file.name.bold().dim(), file.data.len());
    println!("{}", String::from_utf8_lossy(&file.data));
  }

  Ok(())
}
