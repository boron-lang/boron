use crate::Pattern;
use crate::ast::expressions::Expr;
use crate::ast::program::NodeId;
use crate::ast::types::Type;
use boron_utils::prelude::Span;

#[derive(Debug, Clone)]
pub struct Block {
  pub id: NodeId,
  pub statements: Vec<Statement>,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub enum Statement {
  VarDecl(VarDecl),
  Expr(ExprStmt),
  Block(Block),
}

#[derive(Debug, Clone)]
pub struct VarDecl {
  pub id: NodeId,
  pub pat: Pattern,
  pub ty: Option<Type>,
  pub value: Expr,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ExprStmt {
  pub id: NodeId,
  pub expr: Expr,
  pub has_semicolon: bool,
  pub span: Span,
}
