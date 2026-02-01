use crate::cli::try_cli;
use boron_core::prelude::*;
use std::process::exit;

mod cli;

fn main() {
  if let Err(e) = try_cli() {
    error!("{e:?}");
    exit(1);
  }
}
