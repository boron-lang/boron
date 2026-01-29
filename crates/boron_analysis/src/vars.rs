use crate::errors::CannotInferType;
use crate::{InferTy, TyChecker, TyVar, TyVarKind, TypeScheme};
use boron_hir::TyKind::Infer;
use boron_parser::PrimitiveKind;

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
  }

  /// Default unconstrained type variables to concrete types.
  /// Ints default to i32, floats to f64, generals become errs because they couldn't be inferred
  pub fn default_ty_vars(&self, ty: InferTy) -> InferTy {
    self.default_ty_vars_except(ty, &[])
  }

  pub fn default_ty_vars_except(&self, ty: InferTy, except: &[TyVar]) -> InferTy {
    match ty {
      InferTy::Generic(_) => ty,
      InferTy::Var(v, span) => {
        if except.contains(&v) {
          InferTy::Var(v, span)
        } else {
          match self.infcx.var_kind(v) {
            TyVarKind::Integer => InferTy::Primitive(PrimitiveKind::I32, span),
            TyVarKind::Float => InferTy::Primitive(PrimitiveKind::F64, span),
            TyVarKind::General => InferTy::Var(v, span),
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

  pub fn has_unresolved_generics(&self, ty: &InferTy) -> bool {
    match ty {
      InferTy::Generic(_) => true,
      InferTy::Adt { args, .. } => args.iter().any(|t| self.has_unresolved_generics(t)),
      InferTy::Ptr { ty, .. } => self.has_unresolved_generics(ty),
      InferTy::Optional(ty, _) => self.has_unresolved_generics(ty),
      InferTy::Array { ty, .. } => self.has_unresolved_generics(ty),
      InferTy::Slice(ty, _) => self.has_unresolved_generics(ty),
      InferTy::Tuple(tys, _) => tys.iter().any(|t| self.has_unresolved_generics(t)),
      InferTy::Fn { params, ret, .. } => {
        params.iter().any(|t| self.has_unresolved_generics(t)) || self.has_unresolved_generics(ret)
      }
      _ => false,
    }
  }

}
