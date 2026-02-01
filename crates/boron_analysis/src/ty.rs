use boron_parser::ast::types::{Mutability, PrimitiveKind};
use boron_resolver::DefId;
use boron_source::new_id;
use boron_utils::ident_table::Identifier;
use boron_utils::prelude::Span;
use std::collections::HashMap;
use std::fmt::Display;

new_id!(TyVar);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TyVarKind {
  General,
  Integer,
  Float,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TyParam {
  pub def_id: DefId,
  pub name: Identifier,
  pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SubstitutionMap(HashMap<TyParam, InferTy>);

impl SubstitutionMap {
  pub fn new() -> Self {
    Self(HashMap::new())
  }

  pub fn with_values(values: HashMap<TyParam, InferTy>) -> Self {
    Self(values)
  }

  pub fn map(&self) -> &HashMap<TyParam, InferTy> {
    &self.0
  }

  pub fn add(&mut self, ty_param: TyParam, ty: InferTy) {
    self.0.insert(ty_param, ty);
  }

  pub fn get(&self, id: DefId) -> Option<&InferTy> {
    self.0.iter().find(|(p, _)| p.def_id == id).map(|(_, ty)| ty)
  }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InferTy {
  Primitive(PrimitiveKind, Span),
  Adt { def_id: DefId, args: Vec<InferTy>, span: Span },
  Ptr { mutability: Mutability, ty: Box<InferTy>, span: Span },
  Optional(Box<InferTy>, Span),
  Array { ty: Box<InferTy>, len: usize, span: Span },
  Slice(Box<InferTy>, Span),
  Tuple(Vec<InferTy>, Span),
  Fn { params: Vec<InferTy>, ret: Box<InferTy>, span: Span },
  Unit(Span),
  Never(Span),
  Param(TyParam),
  Var(TyVar, Span),
  Err(Span),
}

impl InferTy {
  pub fn span(&self) -> Span {
    match self {
      Self::Var(_, span) => *span,
      Self::Primitive(_, span) => *span,
      Self::Adt { span, .. } => *span,
      Self::Ptr { span, .. } => *span,
      Self::Optional(_, span) => *span,
      Self::Array { span, .. } => *span,
      Self::Slice(_, span) => *span,
      Self::Tuple(_, span) => *span,
      Self::Fn { span, .. } => *span,
      Self::Unit(span) => *span,
      Self::Never(span) => *span,
      Self::Param(param) => param.span,
      Self::Err(span) => *span,
    }
  }

  pub fn has_params(&self) -> bool {
    match self {
      Self::Var(_, _) | Self::Param { .. } => true,
      Self::Primitive(_, _) => false,
      Self::Adt { args, .. } => args.iter().any(|t| t.has_params()),
      Self::Ptr { ty, .. } => ty.has_params(),
      Self::Optional(ty, _) => ty.has_params(),
      Self::Array { ty, .. } => ty.has_params(),
      Self::Slice(ty, _) => ty.has_params(),
      Self::Tuple(tys, _) => tys.iter().any(|t| t.has_params()),
      Self::Fn { params, ret, .. } => {
        params.iter().any(|t| t.has_params()) || ret.has_params()
      }
      Self::Unit(_) | Self::Never(_) | Self::Err(_) => false,
    }
  }

  pub fn is_err(&self) -> bool {
    matches!(self, Self::Err(_))
  }

  pub fn is_var(&self) -> bool {
    matches!(self, Self::Var(_, _))
  }

  pub fn is_numeric(&self) -> bool {
    match self {
      Self::Primitive(p, _) => p.is_numeric(),
      _ => false,
    }
  }

  pub fn is_integer(&self) -> bool {
    match self {
      Self::Primitive(p, _) => p.is_integer(),
      _ => false,
    }
  }

  pub fn is_float(&self) -> bool {
    match self {
      Self::Primitive(p, _) => p.is_float(),
      _ => false,
    }
  }

  pub fn is_bool(&self) -> bool {
    matches!(self, Self::Primitive(PrimitiveKind::Bool, _))
  }

  pub fn is_unit(&self) -> bool {
    matches!(self, Self::Unit(_))
  }

  pub fn is_never(&self) -> bool {
    matches!(self, Self::Never(_))
  }

  pub fn bool(span: Span) -> Self {
    Self::Primitive(PrimitiveKind::Bool, span)
  }
}

#[derive(Debug, Clone)]
pub struct TypeScheme {
  pub vars: Vec<TyParam>,
  pub ty: InferTy,
}

impl TypeScheme {
  pub fn mono(ty: InferTy) -> Self {
    Self { vars: vec![], ty }
  }

  pub fn is_mono(&self) -> bool {
    self.vars.is_empty()
  }
}
