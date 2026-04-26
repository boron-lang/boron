use crate::lowerer::lower_thir_to_blib;
use anyhow::Result;
use boron_context::BCtx;
use boron_session::library::BLibMetadata;
use boron_session::prelude::Session;

pub fn build_blib_metadata<'a>(
  sess: &'a Session,
  ctx: &'a BCtx<'a>,
) -> Result<BLibMetadata> {
  let items = lower_thir_to_blib(sess, ctx);
  Ok(BLibMetadata { config: sess.config.clone(), items })
}
