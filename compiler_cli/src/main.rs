use crate::cli::try_cli;
use std::process::exit;
use zirael_core::prelude::*;

mod cli;

fn main() {
  println!("{:?}", size_of::<Option<*const i32>>());

  if let Err(e) = try_cli() {
    error!("{e:?}");
    exit(1);
  }
}
