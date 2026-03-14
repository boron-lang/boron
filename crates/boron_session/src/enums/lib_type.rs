use serde::{Deserialize, Serialize};
use strum::Display;

#[derive(
  Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug, Display, Deserialize, Serialize,
)]
#[serde(rename_all = "lowercase")]
#[strum(serialize_all = "lowercase")]
pub enum LibType {
  Static,
  Dynamic,
}
