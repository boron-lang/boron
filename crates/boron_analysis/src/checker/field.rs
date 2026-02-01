use crate::checker::TyChecker;
use crate::ty::InferTy;
use boron_utils::ident_table::Identifier;
use boron_utils::prelude::Span;

impl TyChecker<'_> {
  pub(crate) fn check_field_access(
    &self,
    obj_ty: &InferTy,
    field: &Identifier,
    _span: Span,
  ) -> InferTy {
    let resolved = self.infcx.resolve(obj_ty);

    match resolved {
      InferTy::Adt { def_id, span, .. } => {
        if let Some(field_ty) = self.table.field_type(def_id, &field.text()) {
          field_ty
        } else {
          // TODO: Report unknown field error
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
        // TODO: Report error - no fields on this type
        InferTy::Err(_span)
      }
    }
  }
}
