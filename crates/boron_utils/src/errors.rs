use thiserror::Error;

#[derive(Error, Debug)]
// Used for utilities like path functions
pub enum boronError {
  #[error("{0}")]
  IoError(#[from] std::io::Error),
}
