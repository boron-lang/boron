pub mod human_readable;

use crate::Diag;
use std::io::Write;
use std::sync::Arc;
use zirael_source::prelude::Sources;

pub trait Emitter: std::fmt::Debug {
  fn emit_diagnostic(&self, diag: &Diag, w: &mut dyn Write) -> anyhow::Result<()>;

  fn sources(&self) -> &Arc<Sources>;
}
