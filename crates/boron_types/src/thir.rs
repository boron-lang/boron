use crate::ast::{BinaryOp, UnaryOp};
use crate::hir::{HirId, ItemId, Pat};
use crate::infer_ty::InferTy;
use crate::literal_table::FullLiteral;
use boron_source::DefId;
use boron_source::ident_table::Identifier;
use boron_source::prelude::Span;
use dashmap::DashMap;
use dashmap::mapref::one::Ref;

#[derive(Debug, Clone, Default)]
pub struct Generics {
  pub params: Vec<GenericParam>,
}

impl Generics {
  pub fn empty() -> Self {
    Self { params: Vec::new() }
  }

  pub fn is_empty(&self) -> bool {
    self.params.is_empty()
  }
}

#[derive(Debug, Clone)]
pub struct GenericParam {
  pub def_id: DefId,
  pub name: Identifier,
  pub kind: GenericParamKind,
}

#[derive(Debug, Clone)]
pub enum GenericParamKind {
  Type { bounds: Vec<TypeBound> },
  Const { ty: InferTy },
}

#[derive(Debug, Clone)]
pub struct TypeBound {
  pub def_id: DefId,
}

#[derive(Debug, Clone)]
pub struct Function {
  pub def_id: DefId,
  pub name: Identifier,
  pub generics: Generics,
  pub params: Vec<Param>,
  pub return_type: InferTy,
  pub body: Option<Block>,
}

#[derive(Debug, Clone)]
pub struct Param {
  pub name: Identifier,
  pub def_id: DefId,
  pub ty: InferTy,
}

#[derive(Debug, Clone)]
pub struct Struct {
  pub def_id: DefId,
  pub name: Identifier,
  pub generics: Generics,
  pub fields: Vec<Field>,
  pub items: Vec<DefId>,
}

#[derive(Debug, Clone)]
pub struct Field {
  pub name: Identifier,
  pub ty: InferTy,
}

#[derive(Debug, Clone)]
pub struct Module {
  pub hir_id: HirId,
  pub def_id: DefId,
  pub name: Identifier,
  pub items: Vec<ItemId>,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Enum {
  pub def_id: DefId,
  pub name: Identifier,
  pub generics: Generics,
  pub variants: Vec<Variant>,
  pub items: Vec<DefId>,
}

#[derive(Debug, Clone)]
pub struct Variant {
  pub def_id: DefId,
  pub name: Identifier,
  pub kind: VariantKind,
}

#[derive(Debug, Clone)]
pub enum VariantKind {
  Unit,
  Tuple(Vec<InferTy>),
  Struct(Vec<EnumVariantStructField>),
  Discriminant(Expr),
}

#[derive(Debug, Clone)]
pub struct EnumVariantStructField {
  pub name: Identifier,
  pub ty: InferTy,
}

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
    target: Box<Expr>,
    value: Box<Expr>,
  },

  Cast {
    expr: Box<Expr>,
    ty: InferTy,
  },

  Call {
    callee: DefId,
    callee_name: Identifier,
    type_args: Vec<InferTy>,
    args: Vec<Expr>,
  },

  Field {
    object: Box<Expr>,
    field: Identifier,
  },

  Index {
    object: Box<Expr>,
    index: Box<Expr>,
  },

  Struct {
    def_id: DefId,
    type_args: Vec<InferTy>,
    fields: Vec<FieldInit>,
  },

  Tuple(Vec<Expr>),
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

  Loop {
    body: Block,
  },

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
  Expr(Box<Expr>),
}

#[derive(Debug, Clone)]
pub struct Local {
  pub hir_id: HirId,
  pub pat: Pat,
  pub ty: InferTy,
  pub init: Option<Expr>,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct MatchArm {
  pub pat: Pat,
  pub guard: Option<Expr>,
  pub body: Expr,
}

#[derive(Debug, Default)]
pub struct Thir {
  pub functions: DashMap<DefId, Function>,
  pub structs: DashMap<DefId, Struct>,
  pub enums: DashMap<DefId, Enum>,
}

impl Thir {
  pub fn new() -> Self {
    Self::default()
  }

  pub fn get_struct(&self, id: &DefId) -> Ref<'_, DefId, Struct> {
    self.structs.get(id).unwrap()
  }

  pub fn get_function(&self, id: &DefId) -> Ref<'_, DefId, Function> {
    self.functions.get(id).unwrap()
  }

  pub fn get_enum(&self, id: &DefId) -> Ref<'_, DefId, Enum> {
    self.enums.get(id).unwrap()
  }
}
