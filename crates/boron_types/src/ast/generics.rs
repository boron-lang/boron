use crate::ast::{NodeId, Path};
use boron_source::ident_table::Identifier;
use boron_source::prelude::Span;

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
