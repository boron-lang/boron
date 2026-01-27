use strum::{Display, EnumString};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Display, EnumString)]
#[strum(serialize_all = "lowercase")]
pub enum PrimitiveKind {
  // Signed integers
  #[strum(serialize = "i8")]
  I8,
  #[strum(serialize = "i16")]
  I16,
  #[strum(serialize = "i32")]
  I32,
  #[strum(serialize = "i64")]
  I64,
  #[strum(serialize = "i128")]
  I128,
  #[strum(serialize = "isize")]
  ISize,

  // Unsigned integers
  #[strum(serialize = "u8")]
  U8,
  #[strum(serialize = "u16")]
  U16,
  #[strum(serialize = "u32")]
  U32,
  #[strum(serialize = "u64")]
  U64,
  #[strum(serialize = "u128")]
  U128,
  #[strum(serialize = "usize")]
  USize,

  // Floating point
  #[strum(serialize = "f32")]
  F32,
  #[strum(serialize = "f64")]
  F64,

  // Other primitives
  #[strum(serialize = "bool")]
  Bool,
  #[strum(serialize = "char")]
  Char,
}

impl PrimitiveKind {
  pub fn is_integer(&self) -> bool {
    matches!(
      self,
      Self::I8
        | Self::I16
        | Self::I32
        | Self::I64
        | Self::I128
        | Self::ISize
        | Self::U8
        | Self::U16
        | Self::U32
        | Self::U64
        | Self::U128
        | Self::USize
    )
  }

  pub fn is_float(&self) -> bool {
    matches!(self, Self::F32 | Self::F64)
  }

  pub fn is_numeric(&self) -> bool {
    self.is_integer() || self.is_float()
  }

  pub fn is_signed(&self) -> bool {
    matches!(
      self,
      Self::I8 | Self::I16 | Self::I32 | Self::I64 | Self::I128 | Self::ISize
    )
  }

  pub fn is_unsigned(&self) -> bool {
    matches!(
      self,
      Self::U8 | Self::U16 | Self::U32 | Self::U64 | Self::U128 | Self::USize
    )
  }
}
