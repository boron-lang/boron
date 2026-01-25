use std::fmt::{Display, Formatter};
use zirael_parser::ast::types::{Mutability, PrimitiveKind};
use zirael_resolver::DefId;
use zirael_utils::ident_table::Identifier;
use zirael_utils::prelude::Span;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TyVar(pub u32);

impl TyVar {
  pub fn new(id: u32) -> Self {
    Self(id)
  }
}

impl Display for TyVar {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "Type Variable {}", self.0)
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TyVarKind {
  General,
  Integer,
  Float,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InferTy {
  Var(TyVar, Span),
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
      Self::Unit(_) | Self::Never(_) | Self::Param { .. } | Self::Err(_) => false,
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

#[derive(Debug, Clone)]
pub enum UnifyResult {
  Ok,
  Err(UnifyError),
}

impl UnifyResult {
  pub fn is_ok(&self) -> bool {
    matches!(self, Self::Ok)
  }

  pub fn is_err(&self) -> bool {
    matches!(self, Self::Err(_))
  }
}

#[derive(Debug, Clone)]
pub enum UnifyError {
  /// Two concrete types don't match.
  Mismatch { expected: InferTy, found: InferTy },

  /// Occurs check failed (infinite type).
  OccursCheck { var: TyVar, ty: InferTy },

  /// Arity mismatch (e.g., tuple or function parameter count).
  ArityMismatch { expected: usize, found: usize },

  /// Integer type variable couldn't unify with a non-integer type.
  NotAnInteger { ty: InferTy },

  /// Float type variable couldn't unify with a non-float type.
  NotAFloat { ty: InferTy },

  /// Mutability mismatch for pointers.
  MutabilityMismatch { expected: Mutability, found: Mutability },

  /// Array length mismatch.
  ArrayLenMismatch { expected: usize, found: usize },

  /// Incompatible type variable kinds (e.g., Integer vs Float).
  IncompatibleKinds { kind1: TyVarKind, kind2: TyVarKind },
}

#[derive(Debug, Clone)]
pub enum Expectation {
  /// Infer freely.
  None,
  /// A specific type is expected.
  ExpectHasType(InferTy),
  /// A coercible type is acceptable.
  Coercion(InferTy),
}

impl Expectation {
  pub fn none() -> Self {
    Self::None
  }

  pub fn has_type(ty: InferTy) -> Self {
    Self::ExpectHasType(ty)
  }

  pub fn coercion(ty: InferTy) -> Self {
    Self::Coercion(ty)
  }

  pub fn to_option(&self) -> Option<&InferTy> {
    match self {
      Self::None => None,
      Self::ExpectHasType(ty) | Self::Coercion(ty) => Some(ty),
    }
  }

  pub fn is_none(&self) -> bool {
    matches!(self, Self::None)
  }
}
