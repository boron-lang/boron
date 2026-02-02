use std::fmt::Display;

#[derive(Clone, Debug)]
pub enum DiagnosticOutputType {
  HumanReadable,
  Json,
}

impl Display for DiagnosticOutputType {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(
      f,
      "{:?}",
      match self {
        Self::Json => "json",
        Self::HumanReadable => "human readable",
      }
    )
  }
}
