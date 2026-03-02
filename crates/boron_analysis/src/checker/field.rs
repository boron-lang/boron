use crate::checker::TyChecker;
use crate::errors::NoFieldForTy;
use crate::ty::InferTy;
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
        if let Some(field_ty) = self.table.field_type(def_id, *field) {
          let substituted = if let Some(scheme) = self.table.def_type(def_id) {
            Self::substitute_with_scheme(&field_ty, args, scheme)
          } else {
            field_ty
          };

          self.infcx.resolve(&substituted)
        } else {
          self.dcx().emit(NoFieldForTy {
            span,
            field: *field,
            ty: self.format_type(&resolved),
            help: vec![],
          });
          InferTy::Err(span)
        }
      }
      InferTy::Tuple(tys, span) => {
        if let Ok(idx) = field.text().parse::<usize>() {
          if idx < tys.len() {
            return tys[idx].clone();
          }
        }

        self.dcx().emit(NoFieldForTy {
          span: *field.span(),
          field: *field,
          ty: self.format_type(&resolved),
          help: vec![
            "tuples only accept element's index as field. eg: (10, 20).0".to_owned(),
          ],
        });
        InferTy::Err(span)
      }
      _ => {
        if !matches!(resolved, InferTy::Err(_)) {
          self.dcx().emit(NoFieldForTy {
            span,
            field: *field,
            ty: self.format_type(&resolved),
            help: vec![],
          });
        }

        InferTy::Err(span)
      }
    }
  }
}
