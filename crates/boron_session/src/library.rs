use crate::prelude::ProjectConfig;
use boron_source::ident_table::Identifier;
use boron_source::StableDefId;
use boron_target::primitive::PrimitiveKind;
use boron_types::ast::Mutability;
use boron_types::ast::{BinaryOp, UnaryOp};
use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct BLibMetadata {
  pub config: ProjectConfig,
  pub items: Vec<BlibItem>,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub enum BlibItem {
  Function(BlibFunction),
  Struct(BlibStruct),
  Enum(BlibEnum),
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct BlibFunction {
  pub id: StableDefId,
  pub name: Identifier,
  pub generics: BlibGenerics,
  pub params: Vec<BlibParam>,
  pub return_type: BlibTy,
  /// Generic functions need to have a body to be instantiated in downstream projects.
  pub body: Option<BlibBody>,
}

impl BlibFunction {
  pub fn set_body_if_generic(&mut self, body: BlibBody) {
    if !self.generics.params.is_empty() {
      self.body = Some(body);
    }
  }

  pub fn is_generic(&self) -> bool {
    !self.generics.params.is_empty()
  }
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct BlibParam {
  pub name: Identifier,
  pub id: StableDefId,
  pub ty: BlibTy,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct BlibStruct {
  pub id: StableDefId,
  pub name: Identifier,
  pub generics: BlibGenerics,
  pub fields: Vec<BlibField>,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct BlibField {
  pub name: Identifier,
  pub ty: BlibTy,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct BlibEnum {
  pub id: StableDefId,
  pub name: Identifier,
  pub generics: BlibGenerics,
  pub variants: Vec<BlibVariant>,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct BlibVariant {
  pub id: StableDefId,
  pub name: Identifier,
  pub kind: BlibVariantKind,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub enum BlibVariantKind {
  Unit,
  Tuple(Vec<BlibTy>),
  Struct(Vec<BlibEnumField>),
  Discriminant(),
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct BlibEnumField {
  pub name: Identifier,
  pub ty: BlibTy,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum BlibTy {
  Primitive(PrimitiveKind),
  Adt { id: StableDefId, args: Vec<Self> },
  Ptr { mutability: Mutability, ty: Box<Self> },
  Optional(Box<Self>),
  Array { ty: Box<Self>, len: usize },
  Slice(Box<Self>),
  Tuple(Vec<Self>),
  Fn { params: Vec<Self>, ret: Box<Self> },
  Unit,
  Never,
  Param(BlibTyParam),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BlibGenerics {
  pub params: Vec<BlibGenericParam>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BlibGenericParam {
  pub id: StableDefId,
  pub name: Identifier,
  pub kind: BlibGenericParamKind,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum BlibGenericParamKind {
  Type { bounds: Vec<BlibTypeBound> },
  Const { ty: BlibTy },
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BlibTypeBound {
  pub id: StableDefId,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BlibTyParam {
  pub name: Identifier,
  pub index: u32,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BlibBody {
  pub block: BlibBlock,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BlibBlock {
  pub stmts: Vec<BlibStmt>,
  pub expr: Option<Box<BlibExpr>>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum BlibStmt {
  Local(BlibLocal),
  Expr(BlibExpr),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BlibLocal {
  pub id: StableDefId,
  pub name: Identifier,
  pub ty: BlibTy,
  pub init: Option<BlibExpr>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BlibExpr {
  pub ty: BlibTy,
  pub kind: BlibExprKind,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum BlibExprKind {
  Literal(BlibLiteral),

  LocalRef(StableDefId),
  Path(StableDefId),

  Binary {
    op: BinaryOp,
    lhs: Box<BlibExpr>,
    rhs: Box<BlibExpr>,
  },

  Unary {
    op: UnaryOp,
    operand: Box<BlibExpr>,
  },

  Assign {
    target: Box<BlibExpr>,
    value: Box<BlibExpr>,
  },

  Cast {
    expr: Box<BlibExpr>,
    ty: BlibTy,
  },

  Call {
    callee: StableDefId,
    callee_name: Identifier,
    type_args: Vec<BlibTy>,
    args: Vec<BlibExpr>,
  },

  Field {
    object: Box<BlibExpr>,
    field: Identifier,
  },

  Index {
    object: Box<BlibExpr>,
    index: Box<BlibExpr>,
  },

  Struct {
    id: StableDefId,
    type_args: Vec<BlibTy>,
    fields: Vec<BlibFieldInit>,
  },

  Tuple(Vec<BlibExpr>),
  Array(Vec<BlibExpr>),

  Block(BlibBlock),

  If {
    condition: Box<BlibExpr>,
    then_block: BlibBlock,
    else_branch: Option<Box<BlibExpr>>,
  },

  Match {
    scrutinee: Box<BlibExpr>,
    arms: Vec<BlibMatchArm>,
  },

  Loop {
    body: BlibBlock,
  },

  Break {
    value: Option<Box<BlibExpr>>,
  },
  Continue,
  Return {
    value: Option<Box<BlibExpr>>,
  },

  Err,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum BlibLiteral {
  Int(String),
  Float(String),
  Bool(bool),
  Char(char),
  String(String),
  Unit,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BlibFieldInit {
  pub name: Identifier,
  pub ty: BlibTy,
  pub value: BlibExpr,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BlibMatchArm {
  pub body: BlibExpr,
}
