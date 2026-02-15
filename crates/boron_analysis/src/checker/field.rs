use crate::checker::TyChecker;
use crate::errors::NoFieldForTy;
use crate::ty::{InferTy, SubstitutionMap};
use boron_source::ident_table::Identifier;

impl TyChecker<'_> {
  pub(crate) fn check_field_access(
    &self,
    obj_ty: &InferTy,
    field: &Identifier,
  ) -> InferTy {
    let resolved = self.infcx.resolve(obj_ty);
    let span = *field.span();
    match resolved.clone() {
      InferTy::Adt { def_id, args, .. } => {
        if let Some(field_ty) = self.table.field_type(def_id, &field.text()) {
          let substituted = if let Some(scheme) = self.table.def_type(def_id) {
            if !scheme.vars.is_empty() && scheme.vars.len() == args.len() {
              let mut subst = SubstitutionMap::new();
              for (var, arg) in scheme.vars.iter().zip(args.iter()) {
                subst.add(*var, arg.clone());
              }
              Self::apply_subst(&field_ty, &subst)
            } else {
              field_ty
            }
          } else {
            field_ty
          };

          self.infcx.resolve(&substituted)
        } else {
          self.dcx().emit(NoFieldForTy {
            span,
            field: *field,
            ty: self.format_type(&resolved),
          });
          InferTy::Err(span)
        }
      }
      InferTy::Tuple(tys, span) => {
        // Check for tuple field access (e.g., tuple.0)
        if let Ok(idx) = field.text().parse::<usize>() {
          if idx < tys.len() {
            return tys[idx].clone();
          }
        }
        // TODO: Report error
        InferTy::Err(span)
      }
      _ => {
        if !matches!(resolved, InferTy::Err(_)) {
          self.dcx().emit(NoFieldForTy {
            span,
            field: *field,
            ty: self.format_type(&resolved),
          });
        }

        InferTy::Err(span)
      }
    }
  }
}
