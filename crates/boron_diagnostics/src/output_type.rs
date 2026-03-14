use serde::{Deserialize, Serialize};
use strum::Display;

#[derive(Clone, Debug, Display, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub enum DiagnosticOutputType {
  #[strum(serialize = "human readable")]
  HumanReadable,
  #[strum(serialize = "json")]
  Json,
}
