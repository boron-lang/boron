use strum::{Display, EnumString};

#[derive(
  Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug, Default, Display, EnumString,
)]
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
