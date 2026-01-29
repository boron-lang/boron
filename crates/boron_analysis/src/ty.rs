use std::fmt::{Display, Formatter};
use boron_parser::ast::types::{Mutability, PrimitiveKind};
use boron_resolver::DefId;
use boron_source::new_id;
use boron_utils::ident_table::Identifier;
use boron_utils::prelude::Span;

new_id!(TyVar);
new_id!(GenericId);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TyVarKind {
  General,
  Integer,
  Float,
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
  Param { def_id: DefId, name: Identifier, span: Span },
  Var(TyVar, Span),
  Generic(GenericId),
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
      Self::Param { span, .. } => *span,
      Self::Err(span) => *span,
      Self::Generic(..) => Span::dummy(),
    }
  }

  pub fn has_vars(&self) -> bool {
    match self {
      Self::Var(_, _) => true,
      Self::Primitive(_, _) => false,
      Self::Adt { args, .. } => args.iter().any(|t| t.has_vars()),
      Self::Ptr { ty, .. } => ty.has_vars(),
      Self::Optional(ty, _) => ty.has_vars(),
      Self::Array { ty, .. } => ty.has_vars(),
      Self::Slice(ty, _) => ty.has_vars(),
      Self::Tuple(tys, _) => tys.iter().any(|t| t.has_vars()),
      Self::Fn { params, ret, .. } => {
        params.iter().any(|t| t.has_vars()) || ret.has_vars()
      }
      Self::Unit(_)
      | Self::Never(_)
      | Self::Param { .. }
      | Self::Generic(_)
      | Self::Err(_) => false,
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
  pub vars: Vec<TyVar>,
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
