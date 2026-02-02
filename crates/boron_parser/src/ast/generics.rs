use crate::Path;
use boron_utils::prelude::{Identifier, Span};
use crate::ast::program::NodeId;

#[derive(Debug, Clone)]
pub struct GenericParams {
  pub id: NodeId,
  pub params: Vec<GenericParam>,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct GenericParam {
  pub id: NodeId,
  pub name: Identifier,
  pub bounds: Vec<TypeBound>,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct TypeBound {
  pub id: NodeId,
  pub path: Path,
  pub span: Span,
}
