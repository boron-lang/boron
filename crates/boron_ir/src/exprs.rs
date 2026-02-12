use boron_analysis::literal_table::FullLiteral;
use boron_hir::{HirId, SemanticTy};
use boron_parser::{BinaryOp, UnaryOp};
use boron_resolver::DefId;
use boron_session::prelude::Span;
use boron_source::ident_table::Identifier;

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

  Assign { target: Box<IrExpr>, value: Box<IrExpr> },

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
  pub name: String,
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
  Expr(IrExpr),
  Local(HirId),
}

#[derive(Debug, Clone)]
pub struct IrLocal {
  pub hir_id: HirId,
  pub ty: SemanticTy,
  pub init: IrExpr,
  pub span: Span,
  pub projections: Vec<Projection>,
}

#[derive(Debug, Clone)]
pub enum Projection {
  Field { def_id: Option<DefId>, field_idx: u32, struct_ty: SemanticTy },
  Binding(DefId),
}
