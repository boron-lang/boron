use strum::Display;

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug, Display)]
#[strum(serialize_all = "lowercase")]
pub enum LibType {
  Static,
  Dynamic,
}
