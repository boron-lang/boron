use crate::ast::NodeId;
use crate::ast::expressions::Expr;
use crate::ast::import::Path;
use std::fmt::{Display, Formatter};
use boron_utils::prelude::Span;

#[derive(Debug, Clone)]
pub enum Type {
  Primitive(PrimitiveType),
  Path(Path),
  Function(FunctionType),
  Pointer(PointerType),
  Optional(OptionalType),
  Array(ArrayType),
  Tuple(TupleType),
  Unit(UnitType),
  Never(NeverType),
  // most likely means that the parsing failed
  Invalid,
}

#[derive(Debug, Clone)]
pub struct NeverType {
  pub id: NodeId,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct UnitType {
  pub id: NodeId,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct PrimitiveType {
  pub id: NodeId,
  pub kind: PrimitiveKind,
  pub span: Span,
}

pub use boron_target::primitive::PrimitiveKind;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Mutability {
  Mut,
  Const,
}

impl Display for Mutability {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", if matches!(self, Self::Const) { "const" } else { "mut" })
  }
}

#[derive(Debug, Clone)]
pub struct FunctionType {
  pub id: NodeId,
  pub params: Vec<Type>,
  pub return_type: Box<Type>,
  pub span: Span,
  pub is_comptime: bool,
}

#[derive(Debug, Clone)]
pub struct PointerType {
  pub id: NodeId,
  pub mutability: Mutability,
  pub inner: Box<Type>,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct OptionalType {
  pub id: NodeId,
  pub inner: Box<Type>,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ArrayType {
  pub id: NodeId,
  pub element: Box<Type>,
  pub size: Expr,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct TupleType {
  pub id: NodeId,
  pub elements: Vec<Type>,
  pub span: Span,
}

impl Type {
  pub fn span(&self) -> Span {
    match self {
      Self::Primitive(p) => p.span,
      Self::Path(p) => p.span,
      Self::Function(f) => f.span,
      Self::Pointer(r) => r.span,
      Self::Array(a) => a.span,
      Self::Tuple(t) => t.span,
      Self::Unit(u) => u.span,
      Self::Optional(o) => o.span,
      Self::Never(never) => never.span,
      Self::Invalid => Span::dummy(),
    }
  }
}
