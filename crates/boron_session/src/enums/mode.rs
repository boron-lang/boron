use serde::Deserialize;
use strum::{Display, EnumString};

#[derive(
  Copy,
  Clone,
  PartialEq,
  Eq,
  PartialOrd,
  Ord,
  Debug,
  Default,
  Display,
  EnumString,
  Deserialize,
)]
#[serde(rename_all = "lowercase")]
#[strum(serialize_all = "lowercase")]
pub enum Mode {
  #[default]
  Debug,
  Release,
}

impl Mode {
  pub fn from_str(mode: &str) -> Option<Self> {
    mode.parse().ok()
  }
}
