use crate::builtins::{BuiltInFunction, BuiltInParam, get_builtin};
use crate::errors::{ArityMismatch, FuncArgMismatch};
use crate::functions::FinalComptimeArg;
use crate::unify::{Expectation, UnifyError, UnifyResult};
use crate::{InferTy, TyChecker, TypeEnv};
use boron_hir::expr::ComptimeArg;
use boron_hir::{ComptimeCallee, Expr};

impl TyChecker<'_> {
  pub fn check_comptime(
    &mut self,
    expr: &Expr,
    env: &mut TypeEnv,
    callee: &ComptimeCallee,
    args: &Vec<ComptimeArg>,
  ) -> InferTy {
    if let ComptimeCallee::BuiltIn(builtin) = callee {
      let func = get_builtin(builtin);

      if func.params.len() != args.len() {
        self.dcx().emit(ArityMismatch {
          span: expr.span,
          callee: format!("builtin function `{}!`", builtin.clone().name()),
          expected: func.params.len(),
          found: args.len(),
        });

        return InferTy::Err(expr.span);
      }
      let final_args = self.check_builtin_args(func, env, args);

      self.table.comptime_args.insert(expr.hir_id, final_args);
      return func.return_type.clone();
    }

    InferTy::Err(expr.span)
  }

  fn check_builtin_args(
    &mut self,
    func: &BuiltInFunction,
    env: &mut TypeEnv,
    args: &Vec<ComptimeArg>,
  ) -> Vec<FinalComptimeArg> {
    let mut final_args = vec![];
    for (param, arg) in func.params.iter().zip(args) {
      match param {
        BuiltInParam::Type => {
          if let ComptimeArg::Expr(expr) = arg {
            self.dcx().emit(FuncArgMismatch {
              span: expr.span,
              expected: "a comptime type".to_owned(),
              found: "an expression".to_owned(),
            });
          } else if let ComptimeArg::Type(ty) = arg {
            final_args.push(FinalComptimeArg::Ty(self.lower_hir_ty(ty)));
          }
        }

        BuiltInParam::Expr(ty) => {
          if let ComptimeArg::Type(ty) = arg {
            final_args.push(FinalComptimeArg::Ty(self.lower_hir_ty(ty)));
          } else if let ComptimeArg::Expr(expr) = arg {
            let expr_ty =
              self.check_expr(expr, env, &Expectation::ExpectHasType(ty.clone()));
            let result = self.unify(&expr_ty, ty);

            if let UnifyResult::Err(err) = &result {
              if let UnifyError::Mismatch { .. } = err {
                self.dcx().emit(FuncArgMismatch {
                  span: expr.span,
                  expected: self.format_type(ty),
                  found: self.format_type(&expr_ty),
                });
              } else {
                self.handle_unify_result(result, expr.span);
              }
            } else {
              final_args.push(FinalComptimeArg::Expr(*expr.clone()));
            }
          }
        }
      }
    }

    final_args
  }
}
