mod fmt;
pub mod human_readable;
mod show;

use crate::Diag;
use boron_source::prelude::Sources;
use std::io::Write;
use std::sync::Arc;

pub trait Emitter: std::fmt::Debug {
  fn emit_diagnostic(&self, diag: &Diag, w: &mut dyn Write) -> anyhow::Result<()>;

  fn sources(&self) -> &Arc<Sources>;
}
