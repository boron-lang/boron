use crate::ast::expressions::Expr;
use crate::ast::generics::GenericParams;
use crate::ast::params::Param;
use crate::ast::program::{Attribute, NodeId};
use crate::ast::statements::Block;
use crate::ast::types::Type;
use boron_utils::prelude::{Identifier, Span};

#[derive(Debug, Clone)]
pub struct Item {
  pub id: NodeId,
  pub attributes: Vec<Attribute>,
  pub visibility: Visibility,
  pub kind: ItemKind,
  pub span: Span,
  pub doc_comments: Option<Vec<String>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Visibility {
  Public(Span),
  Private,
}

impl Visibility {
  pub fn is_private(&self) -> bool {
    matches!(self, Self::Private)
  }
}

#[derive(Debug, Clone)]
pub enum ItemKind {
  Const(ConstItem),
  Function(FunctionItem),
  Struct(StructItem),
  Enum(EnumItem),
  Mod(ModItem),
}

impl ItemKind {
  pub fn node_id(&self) -> NodeId {
    match self {
      Self::Const(c) => c.id,
      Self::Function(f) => f.id,
      Self::Struct(s) => s.id,
      Self::Enum(e) => e.id,
      Self::Mod(m) => m.id,
    }
  }
}

#[derive(Debug, Clone)]
pub struct ModItem {
  pub id: NodeId,
  pub name: Identifier,
  pub items: Vec<Item>,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ConstItem {
  pub id: NodeId,
  pub name: Identifier,
  pub ty: Type,
  pub value: Expr,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct FunctionItem {
  pub id: NodeId,
  pub is_comptime: bool,
  pub name: Identifier,
  pub generics: Option<GenericParams>,
  pub params: Vec<Param>,
  pub return_type: Type,
  pub body: Option<Block>,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct StructItem {
  pub id: NodeId,
  pub name: Identifier,
  pub generics: Option<GenericParams>,
  pub members: Vec<StructMember>,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub enum StructMember {
  Field(StructField),
  Item(Item),
}

#[derive(Debug, Clone)]
pub struct StructField {
  pub id: NodeId,
  pub attributes: Vec<Attribute>,
  pub visibility: Visibility,
  pub name: Identifier,
  pub ty: Type,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct EnumItem {
  pub id: NodeId,
  pub name: Identifier,
  pub generics: Option<GenericParams>,
  pub variants: Vec<Variant>,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Variant {
  pub id: NodeId,
  pub attributes: Vec<Attribute>,
  pub name: Identifier,
  pub payload: Option<VariantPayload>,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub enum VariantPayload {
  Tuple(Vec<VariantField>),
  Discriminant(Expr),
}

#[derive(Debug, Clone)]
pub enum VariantField {
  Named { name: Identifier, ty: Type },
  Unnamed(Type),
}

#[derive(Debug, Clone)]
pub struct AssociatedType {
  pub id: NodeId,
  pub name: Identifier,
  pub span: Span,
}
