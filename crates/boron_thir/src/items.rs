use crate::exprs::Block;
use boron_analysis::InferTy;
use boron_hir::item::ItemId;
use boron_hir::{Generics, HirId};
use boron_resolver::DefId;
use boron_session::prelude::{Identifier, Span};

#[derive(Debug, Clone)]
pub struct Function {
  pub hir_id: HirId,
  pub def_id: DefId,
  pub name: Identifier,
  pub generics: Generics,
  pub params: Vec<Param>,
  pub return_type: InferTy,
  pub body: Option<Block>,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Param {
  pub hir_id: HirId,
  pub def_id: DefId,
  pub ty: InferTy,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Struct {
  pub hir_id: HirId,
  pub def_id: DefId,
  pub name: Identifier,
  pub generics: Generics,
  pub fields: Vec<Field>,
  pub items: Vec<DefId>,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Field {
  pub hir_id: HirId,
  pub name: Identifier,
  pub ty: InferTy,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Module {
  pub hir_id: HirId,
  pub def_id: DefId,
  pub name: Identifier,
  pub items: Vec<ItemId>,
  pub span: Span,
}
