use serde::{Deserialize, Serialize};
use strum::{Display, EnumString};

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug, Serialize, Deserialize, Display, EnumString)]
#[serde(rename_all = "lowercase")]
#[strum(serialize_all = "lowercase")]
pub enum PackageType {
  #[serde(alias = "bin")]
  #[strum(serialize = "binary", serialize = "bin")]
  Binary,
  #[serde(alias = "lib")]
  #[strum(serialize = "library", serialize = "lib")]
  Library,
}
