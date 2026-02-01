use crate::checker::TyChecker;
use crate::errors::{FieldInitMismatch, InvalidStructInit, NoFieldOnType};
use crate::table::TypeEnv;
use crate::ty::{InferTy, SubstitutionMap};
use crate::unify::{Expectation, UnifyError, UnifyResult};
use boron_hir::Expr;
use boron_hir::expr::FieldInit;
use boron_resolver::{DefId, DefKind};

impl TyChecker<'_> {
  pub(crate) fn check_struct_init(
    &mut self,
    def_id: &DefId,
    fields: &Vec<FieldInit>,
    env: &mut TypeEnv,
    expr: &Expr,
  ) -> InferTy {
    let def = self.resolver.get_definition(*def_id).unwrap();

    if !matches!(def.kind, DefKind::Struct | DefKind::Variant) {
      self.dcx().emit(InvalidStructInit { span: expr.span, found: def.kind.to_string() });
      return InferTy::Err(expr.span);
    }

    let scheme = self.table.def_type(*def_id).unwrap();
    let (def_ty, subst) = self.instantiate(&scheme);

    if DefKind::Struct == def.kind {
      let strukt = self.hir.get_struct(def.id).unwrap().clone();

      for field in fields {
        if !strukt.has_field(field.name) {
          self.dcx().emit(NoFieldOnType {
            span: field.span,
            field: field.name,
            ty: self.format_type(&def_ty),
          });
        } else {
          let field_ty = self.apply_subst(
            &self.table.field_type(*def_id, &field.name.text()).unwrap(),
            &subst,
          );
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
              _ => self.handle_unify_result(result),
            }
          }
        }
      }

      if !scheme.vars.is_empty() {
        let mut resolved_subst = SubstitutionMap::new();
        if let InferTy::Adt { args: scheme_args, .. } = &scheme.ty {
          if let InferTy::Adt { args: inst_args, .. } = &def_ty {
            for (scheme_arg, inst_arg) in scheme_args.iter().zip(inst_args.iter()) {
              self.collect_param_substitutions(
                scheme_arg,
                &self.infcx.resolve(inst_arg),
                &scheme.vars,
                &mut resolved_subst,
              );
            }
          }
        }
        self.table.record_monomorphization(*def_id, resolved_subst);
      }
    }

    def_ty
  }
}
