use serde::Deserialize;
use strum::Display;

#[derive(Clone, Debug, Display, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum DiagnosticOutputType {
  #[strum(serialize = "human readable")]
  HumanReadable,
  #[strum(serialize = "json")]
  Json,
}
