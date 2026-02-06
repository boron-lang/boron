use boron_hir::SemanticTy;
use rustc_apfloat::ieee::{Double, DoubleS, IeeeFloat};
use std::fmt;
use std::fmt::write;

#[derive(Debug, Clone)]
pub enum ConstValue {
  Int(i128),
  Float(IeeeFloat<DoubleS>),
  Bool(bool),
  Char(char),
  String(String),
  Array(Vec<ConstValue>),
  Struct(Vec<ConstValue>),
  Enum { tag: i128, payload: Option<Box<ConstValue>> },
  Type(SemanticTy),
  Unit,
  Poison,
}

impl fmt::Display for ConstValue {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::Int(i) => write!(f, "{i}"),
      Self::Float(d) => write!(f, "{d}"),
      Self::Bool(b) => write!(f, "{b}"),
      Self::Char(c) => write!(f, "'{c}'"),
      Self::String(s) => write!(f, "{s}"),
      Self::Array(arr) => {
        write!(f, "[")?;
        for (i, val) in arr.iter().enumerate() {
          if i > 0 {
            write!(f, ", ")?;
          }
          write!(f, "{val}")?;
        }
        write!(f, "]")
      }
      Self::Struct(fields) => {
        write!(f, "{{")?;
        for (i, val) in fields.iter().enumerate() {
          if i > 0 {
            write!(f, ", ")?;
          }
          write!(f, "{val}")?;
        }
        write!(f, "}}")
      }
      Self::Enum { tag, payload } => {
        if let Some(payload) = payload {
          write!(f, "Enum(tag: {tag}, payload: {payload})")
        } else {
          write!(f, "Enum(tag: {tag})")
        }
      }
      Self::Type(ty) => write!(f, "Type({ty:?})"),
      Self::Unit => write!(f, "()"),
      Self::Poison => write!(f, "invalid value"),
    }
  }
}
