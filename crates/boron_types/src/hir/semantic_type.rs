use boron_target::primitive::PrimitiveKind;
use crate::ast::Mutability;
use crate::resolver::def::DefId;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SemanticTy {
  Primitive(PrimitiveKind),

  Struct { def_id: DefId, args: Vec<Self> },
  Enum { def_id: DefId, args: Vec<Self>, variants: Vec<EnumVariant> },

  Ptr { mutability: Mutability, inner: Box<Self> },
  Optional(Box<Self>),
  Array { elem: Box<Self>, len: usize },
  Slice(Box<Self>),
  Tuple(Vec<Self>),
  Fn { params: Vec<Self>, ret: Box<Self> },

  Unit,
  Never,
  Error,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EnumVariant {
  pub name: String,
  pub discr: i128,
  pub payload: Option<SemanticTy>,
}
