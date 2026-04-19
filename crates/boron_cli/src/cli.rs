use crate::CLAP_STYLING;
use boron_diagnostics::prelude::DiagnosticOutputType;
use boron_session::prelude::{LibType, Mode, PackageType};
use boron_target::target::Compiler;
use clap::{Parser, Subcommand, ValueEnum};
use std::path::PathBuf;

#[derive(Clone, Debug, Subcommand)]
pub enum CliCommand {
  #[command(about = "Deserialize a .blib file and print its contents")]
  InspectBlib {
    #[arg(value_name = "file", help = "Path to the .blib file")]
    file: PathBuf,
  },
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum, Debug, Default)]
pub enum CliMode {
  #[default]
  Debug,
  Release,
}

impl From<CliMode> for Mode {
  fn from(value: CliMode) -> Self {
    match value {
      CliMode::Debug => Self::Debug,
      CliMode::Release => Self::Release,
    }
  }
}

impl From<Mode> for CliMode {
  fn from(value: Mode) -> Self {
    match value {
      Mode::Debug => Self::Debug,
      Mode::Release => Self::Release,
    }
  }
}
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum, Debug)]
pub enum CliPackageType {
  #[value(alias("bin"))]
  Binary,
  #[value(alias("lib"))]
  Library,
}

impl From<CliPackageType> for PackageType {
  fn from(value: CliPackageType) -> Self {
    match value {
      CliPackageType::Binary => Self::Binary,
      CliPackageType::Library => Self::Library,
    }
  }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum, Debug)]
pub enum CliLibType {
  Static,
  Dynamic,
}

impl From<CliLibType> for LibType {
  fn from(value: CliLibType) -> Self {
    match value {
      CliLibType::Static => Self::Static,
      CliLibType::Dynamic => Self::Dynamic,
    }
  }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum, Debug)]
pub enum CliCompiler {
  Clang,
  Gcc,
}

impl From<CliCompiler> for Compiler {
  fn from(value: CliCompiler) -> Self {
    match value {
      CliCompiler::Clang => Self::Clang,
      CliCompiler::Gcc => Self::Gcc,
    }
  }
}

impl From<Compiler> for CliCompiler {
  fn from(value: Compiler) -> Self {
    match value {
      Compiler::Clang => Self::Clang,
      Compiler::Gcc => Self::Gcc,
    }
  }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum, Debug)]
pub enum DiagOutputType {
  Json,
  HumanReadable,
}

impl From<DiagOutputType> for DiagnosticOutputType {
  fn from(value: DiagOutputType) -> Self {
    match value {
      DiagOutputType::Json => Self::Json,
      DiagOutputType::HumanReadable => Self::HumanReadable,
    }
  }
}

#[derive(Parser, Default)]
#[command(name = "boron")]
#[command(bin_name = "boron")]
#[command(styles = CLAP_STYLING)]
pub struct Cli {
  #[command(subcommand)]
  pub command: Option<CliCommand>,

  #[arg(value_name = "project", help = "Path to project.toml")]
  pub project: PathBuf,

  #[arg(value_name = "type", short = 't', long = "type", help = "Type of the project")]
  pub ty: Option<CliPackageType>,

  #[arg(
    value_name = "mode",
    short = 'm',
    long = "mode",
    help = "Compilation mode: either 'debug' or 'release'"
  )]
  pub mode: Option<CliMode>,

  #[arg(
    value_name = "output",
    help = "Path where the codegen should be saved to",
    long = "output",
    short = 'o'
  )]
  pub output: Option<PathBuf>,

  #[arg(
    value_name = "check-only",
    help = "Do not output codegen or compile the code, only check for errors",
    long = "check-only"
  )]
  pub check_only: bool,

  #[arg(value_name = "no-color", help = "No color in the output", long = "no-color")]
  pub no_color: bool,

  #[arg(
    value_name = "no-backtrace",
    help = "No backtrace in panics",
    long = "no-backtrace"
  )]
  pub no_backtrace: bool,

  #[arg(
    value_name = "timings",
    help = "Show timings for each compilation step",
    long = "timings"
  )]
  pub timings: bool,

  #[arg(
    value_name = "verbose",
    short = 'v',
    long = "verbose",
    help = "Enable verbose logging: debug and trace"
  )]
  pub verbose: bool,
}
