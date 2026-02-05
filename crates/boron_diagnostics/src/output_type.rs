use strum::Display;

#[derive(Clone, Debug, Display)]
pub enum DiagnosticOutputType {
  #[strum(serialize = "human readable")]
  HumanReadable,
  #[strum(serialize = "json")]
  Json,
}
