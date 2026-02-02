use crate::{Expr, ImportDecl, Item, Path};
use boron_source::new_id;
use boron_source::prelude::Span;
use boron_utils::ident_table::Identifier;

new_id!(NodeId);

#[derive(Debug, Clone)]
pub struct Attribute {
  pub id: NodeId,
  pub path: AttrPath,
  pub args: Option<Vec<AttrArg>>,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct AttrPath {
  pub id: NodeId,
  pub segments: Vec<Identifier>,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub enum AttrArg {
  Named { name: Identifier, value: Expr },
  Positional(Expr),
}

#[derive(Debug, Clone)]
pub struct ProgramNode {
  pub id: NodeId,
  pub attributes: Vec<Attribute>,
  pub imports: Vec<ImportDecl>,
  pub discover_modules: Vec<Path>,
  pub items: Vec<Item>,
}
