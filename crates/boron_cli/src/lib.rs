mod clap_styles;
mod cli;
pub mod logger;
pub mod project_toml;

pub use clap_styles::CLAP_STYLING;
pub use cli::{Cli, CliLibType, CliMode, CliPackageType};

pub mod prelude {
  pub use crate::cli::*;
  pub use crate::logger::*;
  pub use crate::project_toml::*;
}
