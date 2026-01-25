use crate::{InferTy, TyChecker, TyVar, TyVarKind, TypeScheme};
use zirael_parser::PrimitiveKind;

impl TyChecker<'_> {
  pub fn finalize_types(&mut self) {
    let node_entries: Vec<_> =
      self.table.node_types.iter().map(|e| (*e.key(), e.value().clone())).collect();

    for (hir_id, ty) in node_entries {
      let resolved = self.infcx.resolve(&ty);
      let defaulted = self.default_ty_vars(resolved);
      self.table.node_types.insert(hir_id, defaulted);
    }

    let field_entries: Vec<_> = self
      .table
      .field_types
      .iter()
      .map(|e| (e.key().clone(), e.value().clone()))
      .collect();

    for (key, ty) in field_entries {
      let resolved = self.infcx.resolve(&ty);
      let defaulted = self.default_ty_vars(resolved);
      self.table.field_types.insert(key, defaulted);
    }

    let method_entries: Vec<_> = self
      .table
      .method_types
      .iter()
      .map(|e| (e.key().clone(), e.value().clone()))
      .collect();

    for (key, scheme) in method_entries {
      let finalized_scheme = self.finalize_scheme(scheme);
      self.table.method_types.insert(key, finalized_scheme);
    }

    let def_entries: Vec<_> =
      self.table.def_types.iter().map(|e| (*e.key(), e.value().clone())).collect();

    for (def_id, scheme) in def_entries {
      let finalized_scheme = self.finalize_scheme(scheme);
      self.table.def_types.insert(def_id, finalized_scheme);
    }
  }

  pub fn finalize_scheme(&self, scheme: TypeScheme) -> TypeScheme {
    let resolved = self.infcx.resolve(&scheme.ty);
    // For polymorphic type variables in the scheme, we keep them as-is
    let defaulted = self.default_ty_vars_except(resolved, &scheme.vars);
    TypeScheme { vars: scheme.vars, ty: defaulted }
  }

  /// Default unconstrained type variables to concrete types.
  /// Ints default to i32, floats to f64, generals become errs because they couldn't be inferred
  pub fn default_ty_vars(&self, ty: InferTy) -> InferTy {
    self.default_ty_vars_except(ty, &[])
  }

  pub fn default_ty_vars_except(&self, ty: InferTy, except: &[TyVar]) -> InferTy {
    match ty {
      InferTy::Var(v, span) => {
        if except.contains(&v) {
          InferTy::Var(v, span)
        } else {
          match self.infcx.var_kind(v) {
            TyVarKind::Integer => InferTy::Primitive(PrimitiveKind::I32, span),
            TyVarKind::Float => InferTy::Primitive(PrimitiveKind::F64, span),
            TyVarKind::General => InferTy::Err(span),
          }
        }
      }
      InferTy::Adt { def_id, args, span } => InferTy::Adt {
        def_id,
        args: args.into_iter().map(|t| self.default_ty_vars_except(t, except)).collect(),
        span,
      },
      InferTy::Ptr { mutability, ty, span } => InferTy::Ptr {
        mutability,
        ty: Box::new(self.default_ty_vars_except(*ty, except)),
        span,
      },
      InferTy::Optional(ty, span) => {
        InferTy::Optional(Box::new(self.default_ty_vars_except(*ty, except)), span)
      }
      InferTy::Array { ty, len, span } => InferTy::Array {
        ty: Box::new(self.default_ty_vars_except(*ty, except)),
        len,
        span,
      },
      InferTy::Slice(ty, span) => {
        InferTy::Slice(Box::new(self.default_ty_vars_except(*ty, except)), span)
      }
      InferTy::Tuple(tys, span) => InferTy::Tuple(
        tys.into_iter().map(|t| self.default_ty_vars_except(t, except)).collect(),
        span,
      ),
      InferTy::Fn { params, ret, span } => InferTy::Fn {
        params: params
          .into_iter()
          .map(|t| self.default_ty_vars_except(t, except))
          .collect(),
        ret: Box::new(self.default_ty_vars_except(*ret, except)),
        span,
      },
      InferTy::Primitive(_, _)
      | InferTy::Unit(_)
      | InferTy::Never(_)
      | InferTy::Param { .. }
      | InferTy::Err(_) => ty,
    }
  }
}
