use crate::ast::expressions::Expr;
use crate::ast::program::NodeId;
use crate::ast::types::Type;
use boron_session::prelude::Span;
use boron_source::ident_table::Identifier;

#[derive(Debug, Clone)]
pub enum Param {
  SelfParam(SelfParam),
  Regular(RegularParam),
  Variadic(VariadicParam),
}

#[derive(Debug, Clone)]
pub struct SelfParam {
  pub id: NodeId,
  pub kind: SelfKind,
  pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SelfKind {
  Value,  // self
  Mut,    // mut self
  Ptr,    // *self
  PtrMut, // *mut self
}

#[derive(Debug, Clone)]
pub struct RegularParam {
  pub id: NodeId,
  pub name: Identifier,
  pub ty: Type,
  pub default: Option<Expr>,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct VariadicParam {
  pub id: NodeId,
  pub name: Identifier,
  pub ty: Type,
  pub span: Span,
}
