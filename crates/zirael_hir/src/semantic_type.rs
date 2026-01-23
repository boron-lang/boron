use zirael_parser::{Mutability, PrimitiveKind};
use zirael_resolver::DefId;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SemanticTy {
  Primitive(PrimitiveKind),

  Struct {
    def_id: DefId,
    fields: Vec<SemanticTy>,
  },

  Enum {
    def_id: DefId,
    variants: Vec<EnumVariant>,
  },

  Ptr {
    mutability: Mutability,
    inner: Box<SemanticTy>,
  },

  Optional(Box<SemanticTy>),

  Array {
    elem: Box<SemanticTy>,
    len: usize,
  },

  Slice(Box<SemanticTy>),

  Tuple(Vec<SemanticTy>),

  Fn {
    params: Vec<SemanticTy>,
    ret: Box<SemanticTy>,
  },

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
