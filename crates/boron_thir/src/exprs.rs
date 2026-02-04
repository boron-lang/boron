use boron_analysis::InferTy;
use boron_analysis::literal_table::FullLiteral;
use boron_hir::{HirId, Pat};
use boron_parser::{AssignOp, BinaryOp, UnaryOp};
use boron_resolver::DefId;
use boron_utils::ident_table::Identifier;
use boron_utils::prelude::Span;

#[derive(Debug, Clone)]
pub struct Expr {
  pub hir_id: HirId,
  pub ty: InferTy,
  pub kind: ExprKind,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ExprKind {
  Literal(FullLiteral),

  /// Reference to a local variable or parameter
  LocalRef(DefId),

  Path(DefId),

  Binary {
    op: BinaryOp,
    lhs: Box<Expr>,
    rhs: Box<Expr>,
  },

  Unary {
    op: UnaryOp,
    operand: Box<Expr>,
  },

  Assign {
    op: AssignOp,
    target: Box<Expr>,
    value: Box<Expr>,
  },

  /// Type cast: `x as i32`
  Cast {
    expr: Box<Expr>,
    ty: InferTy,
  },

  Call {
    callee: DefId,
    args: Vec<Expr>,
  },

  /// Field access: `obj.field`
  Field {
    object: Box<Expr>,
    field: Identifier,
  },

  /// Index access: `arr[i]`
  Index {
    object: Box<Expr>,
    index: Box<Expr>,
  },

  /// Address-of: `&x`, `&mut x`
  AddrOf {
    operand: Box<Expr>,
  },

  Struct {
    def_id: DefId,
    fields: Vec<FieldInit>,
  },

  /// Tuple: `(a, b, c)`
  Tuple(Vec<Expr>),

  /// Array: `[1, 2, 3]`
  Array(Vec<Expr>),

  Block(Block),

  If {
    condition: Box<Expr>,
    then_block: Block,
    else_branch: Option<Box<Expr>>,
  },

  Match {
    scrutinee: Box<Expr>,
    arms: Vec<MatchArm>,
  },

  /// Loop: `loop { }`
  Loop {
    body: Block,
  },

  /// Break: `break` or `break value`
  Break {
    value: Option<Box<Expr>>,
  },
  Continue,
  Return {
    value: Option<Box<Expr>>,
  },

  Err,
}

#[derive(Debug, Clone)]
pub struct FieldInit {
  pub hir_id: HirId,
  pub name: Identifier,
  pub ty: InferTy,
  pub value: Expr,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Block {
  pub hir_id: HirId,
  pub ty: InferTy,
  pub stmts: Vec<Stmt>,
  pub expr: Option<Box<Expr>>,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Stmt {
  pub hir_id: HirId,
  pub kind: StmtKind,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub enum StmtKind {
  Local(Box<Local>),
  Expr(Expr),
  /// Expression without semicolon (only valid as last stmt in block)
  Semi(Expr),
}

#[derive(Debug, Clone)]
pub struct Local {
  pub hir_id: HirId,
  pub def_id: DefId,
  pub pat: Pat,
  pub ty: InferTy,
  pub init: Option<Expr>,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct MatchArm {
  pub hir_id: HirId,
  pub pat: Pat,
  pub guard: Option<Expr>,
  pub body: Expr,
  pub span: Span,
}
