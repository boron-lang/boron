use boron_analysis::literal_table::FullLiteral;
use boron_hir::{HirId, SemanticTy};
use boron_parser::{AssignOp, BinaryOp, UnaryOp};
use boron_resolver::DefId;
use boron_utils::ident_table::Identifier;
use boron_utils::prelude::Span;

use crate::cfg::IrTerminator;

#[derive(Debug, Clone)]
pub struct IrExpr {
  pub hir_id: HirId,
  pub ty: SemanticTy,
  pub kind: IrExprKind,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub enum IrExprKind {
  Literal(FullLiteral),

  LocalRef(DefId),
  GlobalRef(DefId),

  Binary { op: BinaryOp, lhs: Box<IrExpr>, rhs: Box<IrExpr> },

  Unary { op: UnaryOp, operand: Box<IrExpr> },

  Assign { op: AssignOp, target: Box<IrExpr>, value: Box<IrExpr> },

  Cast { expr: Box<IrExpr>, ty: SemanticTy },

  Call { callee: DefId, type_args: Vec<SemanticTy>, args: Vec<IrExpr> },

  Field { object: Box<IrExpr>, field: Identifier },

  Index { object: Box<IrExpr>, index: Box<IrExpr> },

  AddrOf { operand: Box<IrExpr> },

  Struct { def_id: DefId, type_args: Vec<SemanticTy>, fields: Vec<IrFieldInit> },

  Tuple(Vec<IrExpr>),
  Array(Vec<IrExpr>),

  Err,
}

#[derive(Debug, Clone)]
pub struct IrFieldInit {
  pub hir_id: HirId,
  pub name: Identifier,
  pub ty: SemanticTy,
  pub value: IrExpr,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct IrBlock {
  pub hir_id: HirId,
  pub stmts: Vec<IrStmt>,
  pub terminator: IrTerminator,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct IrStmt {
  pub hir_id: HirId,
  pub kind: IrStmtKind,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub enum IrStmtKind {
  Local(Box<IrLocal>),
  Expr(IrExpr),
}

#[derive(Debug, Clone)]
pub struct IrLocal {
  pub hir_id: HirId,
  pub def_id: DefId,
  pub ty: SemanticTy,
  pub init: Option<IrExpr>,
  pub span: Span,
}
