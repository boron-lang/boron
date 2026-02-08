use crate::ast::import::Path;
use crate::ast::program::NodeId;
use crate::ast::statements::Block;
use crate::ast::types::{Mutability, Type};
use crate::{IntBase, PrimitiveKind};
use boron_utils::ident_table::Identifier;
use boron_utils::prelude::Span;
use strum::Display;

#[derive(Debug, Clone)]
pub struct Expr {
  pub id: NodeId,
  pub kind: ExprKind,
  pub span: Span,
  pub is_const: bool,
}

#[derive(Debug, Clone)]
pub enum ExprKind {
  // Literals
  Literal(Literal),

  // Identifiers and paths
  Path(Path),
  SelfValue,

  // Binary operations
  Binary { op: BinaryOp, left: Box<Expr>, right: Box<Expr> },

  // Unary operations
  Unary { op: UnaryOp, operand: Box<Expr> },

  // Assignment
  Assign { op: AssignOp, target: Box<Expr>, value: Box<Expr> },

  // Ternary conditional
  Ternary { condition: Box<Expr>, then_expr: Box<Expr>, else_expr: Box<Expr> },

  // Type cast
  Cast { expr: Box<Expr>, target_type: Box<Type> },

  // Function call
  Call { callee: Box<Expr>, args: Vec<Argument> },

  // Field access
  Field { object: Box<Expr>, field: Identifier },

  Index { object: Box<Expr>, index: Box<Expr> },

  AddrOf { mutability: Mutability, operand: Box<Expr> },

  Struct { path: Path, fields: Vec<StructFieldInit> },

  // Control flow
  If(IfExpr),
  Match(MatchExpr),
  Block(Block),
  Loop(LoopExpr),
  While(WhileExpr),
  For(ForExpr),
  Break(BreakExpr),
  Continue(ContinueExpr),
  Return(ReturnExpr),

  Range(RangeExpr),
  Comptime { callee: Box<Expr>, args: Vec<ComptimeArg> },

  // Composite literals
  Tuple(Vec<Expr>),
  Array { values: Vec<Expr>, repeat: Option<Box<Expr>> },
}

impl Expr {
  pub fn new(kind: ExprKind, span: Span) -> Self {
    Self { id: NodeId::new(), kind, span, is_const: false }
  }

  pub fn new_const(kind: ExprKind, span: Span) -> Self {
    Self { id: NodeId::new(), kind, span, is_const: true }
  }

  pub fn dummy() -> Self {
    Self {
      id: NodeId::new(),
      kind: ExprKind::Literal(Literal::Unit(UnitLit {
        id: NodeId::new(),
        span: Span::default(),
      })),
      span: Span::default(),
      is_const: false,
    }
  }
}

#[derive(Debug, Clone)]
pub struct IfExpr {
  pub id: NodeId,
  pub condition: Box<Expr>,
  pub then_block: Block,
  pub else_branch: Option<ElseBranch>,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ElseBranch {
  Block(Block),
  If(Box<IfExpr>),
}

#[derive(Debug, Clone)]
pub struct StructFieldInit {
  pub id: NodeId,
  pub name: Identifier, // .field
  pub value: Expr,      // = value
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct MatchExpr {
  pub id: NodeId,
  pub scrutinee: Box<Expr>,
  pub arms: Vec<MatchArm>,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct LoopExpr {
  pub id: NodeId,
  pub body: Block,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct WhileExpr {
  pub id: NodeId,
  pub condition: Box<Expr>,
  pub body: Block,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ForExpr {
  pub id: NodeId,
  pub binding: Identifier,
  pub iterator: Box<Expr>,
  pub body: Block,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct BreakExpr {
  pub id: NodeId,
  pub value: Option<Box<Expr>>,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ContinueExpr {
  pub id: NodeId,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ReturnExpr {
  pub id: NodeId,
  pub value: Option<Box<Expr>>,
  pub span: Span,
}

// Range expressions: start..end, start..=end, ..end, start.., ..
#[derive(Debug, Clone)]
pub struct RangeExpr {
  pub id: NodeId,
  pub start: Option<Box<Expr>>,
  pub end: Option<Box<Expr>>,
  pub inclusive: bool, // .. vs ..=
  pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ComptimeArg {
  Type(Type),
  Expr(Expr),
}

#[derive(Debug, Clone)]
pub struct MatchArm {
  pub id: NodeId,
  pub pattern: Pattern,
  pub body: Expr,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Argument {
  pub id: NodeId,
  pub name: Option<Identifier>,
  pub value: Expr,
  pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Display)]
pub enum BinaryOp {
  // Arithmetic
  #[strum(serialize = "+")]
  Add,
  #[strum(serialize = "-")]
  Sub,
  #[strum(serialize = "*")]
  Mul,
  #[strum(serialize = "/")]
  Div,
  #[strum(serialize = "%")]
  Mod,

  // Comparison
  #[strum(serialize = "==")]
  Eq,
  #[strum(serialize = "!=")]
  Ne,
  #[strum(serialize = "<")]
  Lt,
  #[strum(serialize = "<=")]
  Le,
  #[strum(serialize = ">")]
  Gt,
  #[strum(serialize = ">=")]
  Ge,

  // Logical
  #[strum(serialize = "&&")]
  And,
  #[strum(serialize = "||")]
  Or,

  // Bitwise
  #[strum(serialize = "&")]
  BitAnd,
  #[strum(serialize = "|")]
  BitOr,
  #[strum(serialize = "^")]
  BitXor,
  #[strum(serialize = "<<")]
  Shl,
  #[strum(serialize = ">>")]
  Shr,
}

impl BinaryOp {
  pub fn precedence(&self) -> u8 {
    match self {
      Self::Or => 1,
      Self::And => 2,
      Self::BitOr => 3,
      Self::BitXor => 4,
      Self::BitAnd => 5,
      Self::Eq | Self::Ne => 6,
      Self::Lt | Self::Le | Self::Gt | Self::Ge => 7,
      Self::Shl | Self::Shr => 8,
      Self::Add | Self::Sub => 9,
      Self::Mul | Self::Div | Self::Mod => 10,
    }
  }

  pub fn is_left_associative(&self) -> bool {
    true
  }
}

impl UnaryOp {
  pub fn precedence(&self) -> u8 {
    2
  }
}

impl AssignOp {
  pub fn precedence(&self) -> u8 {
    14
  }

  pub fn is_left_associative(&self) -> bool {
    false
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Display)]
pub enum UnaryOp {
  #[strum(serialize = "!")]
  Not, // !
  #[strum(serialize = "~")]
  BitNot, // ~
  #[strum(serialize = "-")]
  Neg, // -
  #[strum(serialize = "+")]
  Plus, // +
  #[strum(serialize = "*")]
  Deref, // *
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AssignOp {
  Assign,       // =
  AddAssign,    // +=
  SubAssign,    // -=
  MulAssign,    // *=
  DivAssign,    // /=
  ModAssign,    // %=
  AndAssign,    // &&=
  OrAssign,     // ||=
  BitAndAssign, // &=
  BitOrAssign,  // |=
  BitXorAssign, // ^=
  ShlAssign,    // <<=
  ShrAssign,    // >>=
}

#[derive(Debug, Clone)]
pub struct Pattern {
  pub id: NodeId,
  pub span: Span,
  pub kind: PatternKind,
}

#[derive(Debug, Clone)]
pub enum PatternKind {
  /// Wildcard: `_`
  Wildcard,

  /// Binding: `x`, `mut x`
  Binding {
    name: Identifier,
    is_mut: bool,
    /// `x @ Some(_)`
    subpat: Option<Box<Pattern>>,
  },

  /// `42`, `"hello"`, `true`
  Literal(Literal),

  /// Tuple pattern: `(a, b, c)`
  Tuple(Vec<Pattern>),

  Struct {
    path: Path,
    fields: Vec<FieldPat>,
    rest: bool,
  },

  TupleStruct {
    path: Path,
    patterns: Vec<Pattern>,
    rest: bool,
  },

  /// Path pattern (unit variant, const): `None`, `CONST_VALUE`
  Path(Path),

  /// `A | B | C`
  Or(Vec<Pattern>),

  /// `[first, .., last]`
  Slice {
    prefix: Vec<Pattern>,
    middle: Option<Box<Pattern>>,
    suffix: Vec<Pattern>,
  },

  /// `1..=10`
  Range(RangeExpr),

  Err,
}

#[derive(Debug, Clone)]
pub struct FieldPat {
  pub id: NodeId,
  pub name: Identifier,
  pub pat: Pattern,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub enum Literal {
  Int(IntLit),
  Float(FloatLit),
  String(StringLit),
  Char(CharLit),
  Byte(ByteLit),
  ByteString(ByteStringLit),
  Bool(BoolLit),
  Unit(UnitLit),
}

#[derive(Debug, Clone)]
pub struct UnitLit {
  pub id: NodeId,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct IntLit {
  pub id: NodeId,
  pub value: String,
  pub base: IntBase,
  pub suffix: Option<IntSuffix>,
  pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IntSuffix {
  I8,
  I16,
  I32,
  I64,
  I128,
  ISize,
  U8,
  U16,
  U32,
  U64,
  U128,
  USize,
}

impl IntSuffix {
  pub fn to_primitive_kind(&self) -> PrimitiveKind {
    match self {
      Self::I8 => PrimitiveKind::I8,
      Self::I16 => PrimitiveKind::I16,
      Self::I32 => PrimitiveKind::I32,
      Self::I64 => PrimitiveKind::I64,
      Self::I128 => PrimitiveKind::I128,
      Self::ISize => PrimitiveKind::ISize,
      Self::U8 => PrimitiveKind::U8,
      Self::U16 => PrimitiveKind::U16,
      Self::U32 => PrimitiveKind::U32,
      Self::U64 => PrimitiveKind::U64,
      Self::U128 => PrimitiveKind::U128,
      Self::USize => PrimitiveKind::USize,
    }
  }

  pub fn parse_int_suffix(lexeme: &str, value: &str) -> Option<Self> {
    let suffix_str = lexeme.strip_prefix(value).unwrap_or("");
    let suffix_str = suffix_str.trim_start_matches(|c: char| {
      c == 'x'
        || c == 'X'
        || c == 'b'
        || c == 'B'
        || c == 'o'
        || c == 'O'
        || c.is_ascii_hexdigit()
        || c == '_'
    });

    match suffix_str {
      "i8" => Some(Self::I8),
      "i16" => Some(Self::I16),
      "i32" => Some(Self::I32),
      "i64" => Some(Self::I64),
      "i128" => Some(Self::I128),
      "isize" => Some(Self::ISize),
      "u8" => Some(Self::U8),
      "u16" => Some(Self::U16),
      "u32" => Some(Self::U32),
      "u64" => Some(Self::U64),
      "u128" => Some(Self::U128),
      "usize" => Some(Self::USize),
      _ => None,
    }
  }
}

#[derive(Debug, Clone)]
pub struct FloatLit {
  pub id: NodeId,
  pub value: String,
  pub suffix: Option<FloatSuffix>,
  pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FloatSuffix {
  F32,
  F64,
}

impl FloatSuffix {
  pub fn to_primitive_kind(&self) -> PrimitiveKind {
    match self {
      Self::F32 => PrimitiveKind::F32,
      Self::F64 => PrimitiveKind::F64,
    }
  }
}

#[derive(Debug, Clone)]
pub struct StringLit {
  pub id: NodeId,
  pub value: String,
  pub raw: String,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct CharLit {
  pub id: NodeId,
  pub value: char,
  pub raw: String,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ByteLit {
  pub id: NodeId,
  pub value: u8,
  pub raw: String,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct BoolLit {
  pub id: NodeId,
  pub value: bool,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ByteStringLit {
  pub id: NodeId,
  pub value: Vec<u8>,
  pub raw: String,
  pub span: Span,
}
