use crate::ty::ArrayLength;
use crate::{InferTy, TyChecker, TyVar, TyVarKind};
use boron_parser::Mutability;

impl TyChecker<'_> {
  #[must_use = "the caller should handle the result"]
  pub fn unify(&self, a: &InferTy, b: &InferTy) -> UnifyResult {
    let a = self.infcx.resolve(a);
    let b = self.infcx.resolve(b);

    match (&a, &b) {
      _ if a.semantically_eq(&b) => UnifyResult::Ok,

      (InferTy::Err(_) | InferTy::Never(_), _)
      | (_, InferTy::Err(_) | InferTy::Never(_)) => UnifyResult::Ok,

      (InferTy::Var(v1, _), InferTy::Var(v2, _)) => {
        let k1 = self.infcx.var_kind(*v1);
        let k2 = self.infcx.var_kind(*v2);

        match (k1, k2) {
          (TyVarKind::Integer | TyVarKind::Float, TyVarKind::General) => {
            self.infcx.unify_var(*v2, a.clone());
          }
          (TyVarKind::Integer, TyVarKind::Float)
          | (TyVarKind::Float, TyVarKind::Integer) => {
            return UnifyResult::Err(UnifyError::IncompatibleKinds {
              kind1: k1,
              kind2: k2,
            });
          }
          _ => {
            self.infcx.unify_var(*v1, b.clone());
          }
        }
        UnifyResult::Ok
      }

      (InferTy::Var(v, span), t) | (t, InferTy::Var(v, span)) => {
        if t.has_params() {
          // TODO: Proper occurs check
        }

        let kind = self.infcx.var_kind(*v);
        match kind {
          TyVarKind::Integer => {
            if !t.is_integer() {
              return UnifyResult::Err(UnifyError::Mismatch {
                expected: t.clone(),
                found: InferTy::Var(*v, *span),
              });
            }
          }
          TyVarKind::Float => {
            if !t.is_float() {
              return UnifyResult::Err(UnifyError::Mismatch {
                expected: t.clone(),
                found: InferTy::Var(*v, *span),
              });
            }
          }
          TyVarKind::General => {}
        }

        self.infcx.unify_var(*v, t.clone());
        UnifyResult::Ok
      }

      (
        InferTy::Ptr { mutability: m1, ty: t1, .. },
        InferTy::Ptr { mutability: m2, ty: t2, .. },
      ) => {
        if m1 != m2 {
          //todo: with span
          return UnifyResult::Err(UnifyError::MutabilityMismatch {
            expected: *m1,
            found: *m2,
          });
        }
        self.unify(t1, t2)
      }

      (InferTy::Optional(t1, _), InferTy::Optional(t2, _))
      | (InferTy::Slice(t1, _), InferTy::Slice(t2, _)) => self.unify(t1, t2),

      (
        InferTy::Array { ty: t1, len: l1, .. },
        InferTy::Array { ty: t2, len: l2, .. },
      ) => {
        if let ArrayLength::Len(l1) = l1
          && let ArrayLength::Len(l2) = l2
        {
          if l1 != l2 {
            return UnifyResult::Err(UnifyError::ArrayLenMismatch {
              expected: *l1,
              found: *l2,
            });
          }
          self.unify(t1, t2)
        } else {
          return UnifyResult::Ok;
        }
      }

      (InferTy::Tuple(ts1, ..), InferTy::Tuple(ts2, ..)) => {
        if ts1.len() != ts2.len() {
          return UnifyResult::Err(UnifyError::ArityMismatch {
            expected: ts1.len(),
            found: ts2.len(),
          });
        }
        for (t1, t2) in ts1.iter().zip(ts2.iter()) {
          let result = self.unify(t1, t2);
          if result.is_err() {
            return result;
          }
        }
        UnifyResult::Ok
      }

      (
        InferTy::Fn { params: p1, ret: r1, .. },
        InferTy::Fn { params: p2, ret: r2, .. },
      ) => {
        if p1.len() != p2.len() {
          return UnifyResult::Err(UnifyError::ArityMismatch {
            expected: p1.len(),
            found: p2.len(),
          });
        }
        for (t1, t2) in p1.iter().zip(p2.iter()) {
          let result = self.unify(t1, t2);
          if result.is_err() {
            return result;
          }
        }
        self.unify(r1, r2)
      }

      (
        InferTy::Adt { def_id: d1, args: a1, .. },
        InferTy::Adt { def_id: d2, args: a2, .. },
      ) => {
        if d1 != d2 {
          return UnifyResult::Err(UnifyError::Mismatch {
            expected: a.clone(),
            found: b.clone(),
          });
        }
        if a1.len() != a2.len() {
          return UnifyResult::Err(UnifyError::ArityMismatch {
            expected: a1.len(),
            found: a2.len(),
          });
        }
        for (t1, t2) in a1.iter().zip(a2.iter()) {
          let result = self.unify(t1, t2);
          if result.is_err() {
            return result;
          }
        }
        UnifyResult::Ok
      }

      _ => UnifyResult::Err(UnifyError::Mismatch { expected: a, found: b }),
    }
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
