use num_bigint::BigInt;
use std::fmt;
use zirael_hir::SemanticTy;

#[derive(Debug, Clone)]
pub enum ConstValue {
  Int(i128),
  Bool(bool),
  Char(char),
  String(String),
  Array(Vec<ConstValue>),
  Struct(Vec<ConstValue>),
  Enum {
    tag: i128,
    payload: Option<Box<ConstValue>>,
  },
  Type(SemanticTy),
  Unit,
  Poison,
}

impl fmt::Display for ConstValue {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      ConstValue::Int(i) => write!(f, "{}", i),
      ConstValue::Bool(b) => write!(f, "{}", b),
      ConstValue::Char(c) => write!(f, "'{}'", c),
      ConstValue::String(s) => write!(f, "{}", s),
      ConstValue::Array(arr) => {
        write!(f, "[")?;
        for (i, val) in arr.iter().enumerate() {
          if i > 0 {
            write!(f, ", ")?;
          }
          write!(f, "{}", val)?;
        }
        write!(f, "]")
      }
      ConstValue::Struct(fields) => {
        write!(f, "{{")?;
        for (i, val) in fields.iter().enumerate() {
          if i > 0 {
            write!(f, ", ")?;
          }
          write!(f, "{}", val)?;
        }
        write!(f, "}}")
      }
      ConstValue::Enum { tag, payload } => {
        if let Some(payload) = payload {
          write!(f, "Enum(tag: {}, payload: {})", tag, payload)
        } else {
          write!(f, "Enum(tag: {})", tag)
        }
      }
      ConstValue::Type(ty) => write!(f, "Type({:?})", ty),
      ConstValue::Unit => write!(f, "()"),
      ConstValue::Poison => write!(f, "invalid value"),
    }
  }
}
