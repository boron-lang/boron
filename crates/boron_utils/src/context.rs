use crate::prelude::*;
use boron_diagnostics::DiagnosticCtx;
use boron_source::sources::Sources;
use boron_target::target::Target;
use std::sync::Arc;

pub struct Context<'ctx> {
  pub session: &'ctx Session,
  pub sources: Arc<Sources>,
}

impl<'ctx> Context<'ctx> {
  pub fn new(session: &'ctx Session, sources: Arc<Sources>) -> Self {
    Context { session, sources }
  }

  pub fn dcx(&self) -> &DiagnosticCtx {
    self.session.dcx()
  }

  pub fn target(&self) -> &Target {
    self.session.target()
  }
}
