use crate::ast::Mutability;
use crate::resolver::def::DefId;
use boron_source::ident_table::Identifier;
use boron_source::new_id;
use boron_source::prelude::Span;
use boron_target::primitive::PrimitiveKind;
use std::collections::HashMap;

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

impl Default for SubstitutionMap {
  fn default() -> Self {
    Self::new()
  }
}

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
  Adt { def_id: DefId, args: Vec<Self>, span: Span },
  Ptr { mutability: Mutability, ty: Box<Self>, span: Span },
  Optional(Box<Self>, Span),
  Array { ty: Box<Self>, len: ArrayLength, span: Span },
  Slice(Box<Self>, Span),
  Tuple(Vec<Self>, Span),
  Fn { params: Vec<Self>, ret: Box<Self>, span: Span },
  Unit(Span),
  Never(Span),
  Param(TyParam),
  Var(TyVar, Span),
  Err(Span),
}

#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub enum ArrayLength {
  Len(usize),
  Poisoned,
}

impl ArrayLength {
  pub fn len(&self) -> usize {
    match self {
      Self::Len(num) => *num,
      Self::Poisoned => 0,
    }
  }

  pub fn expect_len(&self) -> usize {
    match self {
      Self::Len(len) => *len,
      Self::Poisoned => unreachable!("compilation should be stopped before"),
    }
  }
}

impl InferTy {
  pub fn span(&self) -> Span {
    match self {
      Self::Var(_, span)
      | Self::Primitive(_, span)
      | Self::Adt { span, .. }
      | Self::Ptr { span, .. }
      | Self::Optional(_, span)
      | Self::Array { span, .. }
      | Self::Slice(_, span)
      | Self::Tuple(_, span)
      | Self::Fn { span, .. }
      | Self::Unit(span)
      | Self::Never(span)
      | Self::Err(span) => *span,

      Self::Param(param) => param.span,
    }
  }

  pub fn has_params(&self) -> bool {
    match self {
      Self::Var(_, _) | Self::Param { .. } => true,
      Self::Adt { args, .. } => args.iter().any(|t| t.has_params()),
      Self::Ptr { ty, .. }
      | Self::Slice(ty, _)
      | Self::Optional(ty, _)
      | Self::Array { ty, .. } => ty.has_params(),
      Self::Tuple(tys, _) => tys.iter().any(|t| t.has_params()),
      Self::Fn { params, ret, .. } => {
        params.iter().any(|t| t.has_params()) || ret.has_params()
      }
      Self::Primitive(_, _) | Self::Unit(_) | Self::Never(_) | Self::Err(_) => false,
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

  pub fn apply_subst(&self, subst: &SubstitutionMap) -> Self {
    match self {
      Self::Var(_, _) => self.clone(),
      Self::Param(p) => subst.get(p.def_id).cloned().unwrap_or_else(|| self.clone()),
      Self::Adt { def_id, args, span } => Self::Adt {
        def_id: *def_id,
        args: args.iter().map(|t| t.apply_subst(subst)).collect(),
        span: *span,
      },
      Self::Ptr { mutability, ty, span } => Self::Ptr {
        mutability: *mutability,
        ty: Box::new(ty.apply_subst(subst)),
        span: *span,
      },
      Self::Optional(ty, span) => Self::Optional(Box::new(ty.apply_subst(subst)), *span),
      Self::Array { ty, len, span } => {
        Self::Array { ty: Box::new(ty.apply_subst(subst)), len: *len, span: *span }
      }
      Self::Slice(ty, span) => Self::Slice(Box::new(ty.apply_subst(subst)), *span),
      Self::Tuple(tys, span) => {
        Self::Tuple(tys.iter().map(|t| t.apply_subst(subst)).collect(), *span)
      }
      Self::Fn { params, ret, span } => Self::Fn {
        params: params.iter().map(|t| t.apply_subst(subst)).collect(),
        ret: Box::new(ret.apply_subst(subst)),
        span: *span,
      },
      _ => self.clone(),
    }
  }

  pub fn semantically_eq(&self, other: &Self) -> bool {
    match (self, other) {
      (Self::Primitive(k1, _), Self::Primitive(k2, _)) => k1 == k2,
      (Self::Unit(_), Self::Unit(_))
      | (Self::Never(_), Self::Never(_))
      | (Self::Err(_), Self::Err(_)) => true,
      (Self::Var(v1, _), Self::Var(v2, _)) => v1 == v2,
      (Self::Param(p1), Self::Param(p2)) => p1.def_id == p2.def_id,
      (
        Self::Adt { def_id: d1, args: a1, .. },
        Self::Adt { def_id: d2, args: a2, .. },
      ) => {
        d1 == d2
          && a1.len() == a2.len()
          && a1.iter().zip(a2.iter()).all(|(t1, t2)| t1.semantically_eq(t2))
      }
      (
        Self::Ptr { mutability: m1, ty: t1, .. },
        Self::Ptr { mutability: m2, ty: t2, .. },
      ) => m1 == m2 && t1.semantically_eq(t2),
      (Self::Optional(t1, _), Self::Optional(t2, _))
      | (Self::Slice(t1, _), Self::Slice(t2, _)) => t1.semantically_eq(t2),
      (Self::Array { ty: t1, len: l1, .. }, Self::Array { ty: t2, len: l2, .. }) => {
        l1 == l2 && t1.semantically_eq(t2)
      }
      (Self::Tuple(ts1, _), Self::Tuple(ts2, _)) => {
        ts1.len() == ts2.len()
          && ts1.iter().zip(ts2.iter()).all(|(t1, t2)| t1.semantically_eq(t2))
      }
      (Self::Fn { params: p1, ret: r1, .. }, Self::Fn { params: p2, ret: r2, .. }) => {
        p1.len() == p2.len()
          && p1.iter().zip(p2.iter()).all(|(t1, t2)| t1.semantically_eq(t2))
          && r1.semantically_eq(r2)
      }
      _ => false,
    }
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

  pub fn substitution_for_args(&self, args: &[InferTy]) -> Option<SubstitutionMap> {
    if self.vars.len() != args.len() {
      return None;
    }

    let mut subst = SubstitutionMap::new();
    for (var, arg) in self.vars.iter().zip(args.iter()) {
      subst.add(*var, arg.clone());
    }
    Some(subst)
  }
}
