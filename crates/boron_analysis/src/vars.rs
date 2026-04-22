use crate::{InferTy, TyChecker, TyVar, TyVarKind};
use boron_target::primitive::PrimitiveKind;
use boron_types::infer_ty::SubstitutionMap;
use boron_types::type_table::MonomorphizationEntry;

impl TyChecker<'_> {
  pub fn finalize_types(&self) {
    let node_entries: Vec<_> =
      self.ctx.tt().node_types.iter().map(|e| (*e.key(), e.value().clone())).collect();

    for (hir_id, ty) in node_entries {
      let resolved = self.infcx.resolve(&ty);
      let defaulted = self.default_ty_vars(resolved);
      self.ctx.record_node_type(hir_id, defaulted);
    }

    let field_entries: Vec<_> =
      self.ctx.tt().field_types.iter().map(|e| (*e.key(), e.value().clone())).collect();

    for ((def_id, ident), ty) in field_entries {
      let resolved = self.infcx.resolve(&ty);
      let defaulted = self.default_ty_vars(resolved);
      self.ctx.record_field_type(def_id, ident, defaulted);
    }

    let mono_entries: Vec<_> = self
      .ctx
      .tt()
      .monomorphizations
      .iter()
      .map(|e| (*e.key(), e.value().clone()))
      .collect();

    for (def_id, entries) in mono_entries {
      let resolved_entries: Vec<_> = entries
        .into_iter()
        .map(|entry| {
          let resolved_type_args = entry
            .type_args
            .map()
            .iter()
            .map(|(param_def_id, ty)| {
              let resolved = self.infcx.resolve(ty);
              let defaulted = self.default_ty_vars(resolved);
              (*param_def_id, defaulted)
            })
            .collect();

          let resolved_type_args = SubstitutionMap::with_values(resolved_type_args);
          MonomorphizationEntry { def_id: entry.def_id, type_args: resolved_type_args }
        })
        .collect();
      self.ctx.tt().monomorphizations.insert(def_id, resolved_entries);
    }

    let expr_mono_entries: Vec<_> = self
      .ctx
      .tt()
      .expr_monomorphizations
      .iter()
      .map(|e| (*e.key(), e.value().clone()))
      .collect();

    for (hir_id, entry) in expr_mono_entries {
      let resolved_type_args = entry
        .type_args
        .map()
        .iter()
        .map(|(param_def_id, ty)| {
          let resolved = self.infcx.resolve(ty);
          let defaulted = self.default_ty_vars(resolved);
          (*param_def_id, defaulted)
        })
        .collect();

      let resolved_type_args = SubstitutionMap::with_values(resolved_type_args);
      self.ctx.record_expr_mono(hir_id, entry.def_id, resolved_type_args);
    }
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
            TyVarKind::General => ty,
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
