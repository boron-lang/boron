use std::str::FromStr as _;
use strum::EnumString;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, Default, EnumString)]
pub enum Abi {
  /// The C ABI is considered the default
  #[default]
  #[strum(serialize = "C")]
  C,
}

impl Abi {
  pub fn parse_from_string(abi: String) -> Option<Self> {
    Self::from_str(&abi).ok()
  }
}
