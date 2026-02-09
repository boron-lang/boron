use crate::cli::try_cli;
use boron_core::prelude::*;
use std::process::exit;

mod cli;

struct Vec2 {
  x: f32,
  y: f32,
}

fn main() {
  if let Err(e) = try_cli() {
    error!("{e:?}");
    exit(1);
  }
}
