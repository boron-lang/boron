pub mod builtins;
pub mod expander;
pub mod functions;
pub mod results;

use crate::builtin::expander::BuiltInExpander;
use crate::results::BuiltInResults;
use boron_context::BCtx;
use boron_session::prelude::Session;

pub struct BuiltinFunctionCtx<'sess, 'ctx> {
  pub sess: &'sess Session,
  pub ctx: &'sess BCtx<'ctx>,
}

pub fn expand_builtins<'a>(sess: &'a Session, ctx: &'a BCtx<'a>) -> BuiltInResults {
  let expander = BuiltInExpander::new(sess, ctx);

  for func in &ctx.hir().functions {
    expander.expand_function(&func);
  }

  expander.results
}
