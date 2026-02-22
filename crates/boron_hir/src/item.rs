use crate::Expr;
use crate::expr::Block;
use crate::generics::Generics;
use crate::ids::HirId;
use crate::ty::Ty;
use boron_parser::FunctionModifiers;
use boron_parser::ast::items::Visibility;
use boron_resolver::DefId;
use boron_session::prelude::{Identifier, Span};

#[derive(Debug, Clone)]
pub struct Function {
  pub hir_id: HirId,
  pub def_id: DefId,
  pub name: Identifier,
  pub visibility: Visibility,
  pub modifiers: FunctionModifiers,
  pub generics: Generics,
  pub params: Vec<Param>,
  pub return_type: Ty,
  pub body: Option<Block>,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Param {
  pub hir_id: HirId,
  pub def_id: DefId,
  pub kind: ParamKind,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ParamKind {
  /// Regular named parameter: `name: Type`
  Regular { name: Identifier, ty: Ty, default: Option<Expr> },
  /// Self parameter: `self`, `mut self`, `*self`, `*mut self`
  SelfParam { kind: SelfKind },
  /// Variadic parameter: `...args: Type`
  Variadic { name: Identifier, ty: Ty },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SelfKind {
  Value,
  Mut,
  Ptr,
  PtrMut,
}

#[derive(Debug, Clone)]
pub struct Struct {
  pub hir_id: HirId,
  pub def_id: DefId,
  pub name: Identifier,
  pub visibility: Visibility,
  pub generics: Generics,
  pub fields: Vec<Field>,
  pub items: Vec<DefId>,
  pub span: Span,
}

impl Struct {
  pub fn has_field(&self, field: Identifier) -> bool {
    self.fields.iter().any(|f| f.name == field)
  }
}

#[derive(Debug, Clone)]
pub struct Field {
  pub hir_id: HirId,
  pub name: Identifier,
  pub visibility: Visibility,
  pub ty: Ty,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Enum {
  pub hir_id: HirId,
  pub def_id: DefId,
  pub name: Identifier,
  pub visibility: Visibility,
  pub generics: Generics,
  pub variants: Vec<Variant>,
  pub items: Vec<DefId>,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Variant {
  pub hir_id: HirId,
  pub def_id: DefId,
  pub name: Identifier,
  pub kind: VariantKind,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub enum VariantKind {
  Unit,
  Tuple(Vec<Ty>),
  Struct(Vec<EnumVariantStructField>),
  Discriminant(Expr),
}

#[derive(Debug, Clone)]
pub struct EnumVariantStructField {
  pub id: HirId,
  pub name: Identifier,
  pub ty: Ty,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Const {
  pub hir_id: HirId,
  pub def_id: DefId,
  pub name: Identifier,
  pub visibility: Visibility,
  pub ty: Ty,
  pub value: Expr,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Module {
  pub hir_id: HirId,
  pub def_id: DefId,
  pub name: Identifier,
  pub visibility: Visibility,
  pub items: Vec<ItemId>,
  pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ItemId(pub DefId);
