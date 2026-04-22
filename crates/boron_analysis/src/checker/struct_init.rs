use crate::checker::TyChecker;
use crate::errors::{FieldInitMismatch, InvalidStructInit, NoFieldForStructInit};
use crate::table::TypeEnv;
use crate::unify::{Expectation, UnifyError, UnifyResult};
use boron_resolver::{DefId, DefKind};
use boron_types::hir::{Expr, FieldInit};
use boron_types::infer_ty::InferTy;

impl TyChecker<'_> {
  pub(crate) fn check_struct_init(
    &mut self,
    def_id: &DefId,
    fields: &Vec<FieldInit>,
    env: &mut TypeEnv,
    expr: &Expr,
  ) -> InferTy {
    let def = self.ctx.get_definition(*def_id).unwrap();

    if !matches!(def.kind, DefKind::Struct | DefKind::Variant) {
      self.dcx().emit(InvalidStructInit { span: expr.span, found: def.kind.to_string() });
      return InferTy::Err(expr.span);
    }

    let scheme = self.ctx.def_type(*def_id).unwrap();
    let (def_ty, subst) = self.instantiate(&scheme);

    if let Some(strukt) = self.ctx.hir_struct(def.id).as_ref() {
      for field in fields {
        if !strukt.has_field(field.name) {
          self.dcx().emit(NoFieldForStructInit {
            span: field.span,
            field: field.name,
            ty: self.format_type(&def_ty),
          });
        } else {
          let field_ty =
            Self::apply_subst(&self.ctx.field_type(*def_id, field.name).unwrap(), &subst);
          let arg_ty =
            self.check_expr(&field.value, env, &Expectation::has_type(field_ty.clone()));

          let result = self.unify(&arg_ty, &field_ty);
          if let UnifyResult::Err(err) = &result {
            match err {
              UnifyError::Mismatch { .. } => self.dcx().emit(FieldInitMismatch {
                expected: self.format_type(&field_ty),
                found: self.format_type(&arg_ty),
                span: field.span,
              }),
              _ => self.handle_unify_result(result, field.span),
            }
          }
        }
      }

      let resolved_subst = self.substitutions_from_instantiation(&scheme, &def_ty);
      if !scheme.vars.is_empty() {
        self.ctx.record_mono(*def_id, resolved_subst.clone());
      }

      self.ctx.record_expr_mono(expr.hir_id, *def_id, resolved_subst);
    }

    def_ty
  }
}
