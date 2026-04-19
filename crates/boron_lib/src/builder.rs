use anyhow::Result;
use boron_session::library::BLibMetadata;
use boron_session::prelude::{ProjectConfig, Session};
use boron_target::target::Target;
use postcard::{from_bytes, to_allocvec};
use serde::{Deserialize, Serialize};

#[derive(Serialize)]
pub struct BLibMetadataRef<'a> {
  config: &'a ProjectConfig,
  target: &'a Target,
}

#[derive(Deserialize)]
struct BLibMetadataCore {
  config: ProjectConfig,
  target: Target,
}

pub fn build_blib_metadata(sess: &Session) -> Result<BLibMetadata> {
  let metadata_ref = BLibMetadataRef { config: sess.config(), target: sess.target() };
  let bytes = to_allocvec(&metadata_ref)?;
  let core: BLibMetadataCore = from_bytes(&bytes)?;
  Ok(BLibMetadata::from_core(core.config, core.target))
}
