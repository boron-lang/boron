use boron_session::project_config::ProjectConfig;
use boron_target::target::Target;
use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize)]
pub struct BLibMetadata {
  pub config: ProjectConfig,
  pub target: Target,
}

impl BLibMetadata {}
