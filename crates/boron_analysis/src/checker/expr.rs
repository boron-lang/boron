use crate::builtins::{BuiltInParam, get_builtin};
use crate::checker::TyChecker;
use crate::errors::{ArityMismatch, FuncArgMismatch};
use crate::functions::FinalComptimeArg;
use crate::table::TypeEnv;
use crate::ty::InferTy;
use crate::unify::{Expectation, UnifyError, UnifyResult};
use boron_hir::expr::ComptimeArg;
use boron_hir::{Expr, ExprKind, Literal};
use boron_parser::Mutability;
use boron_parser::ast::types::PrimitiveKind;
use boron_utils::prelude::{Span, warn};

impl TyChecker<'_> {
  pub(crate) fn check_expr(
    &mut self,
    expr: &Expr,
    env: &mut TypeEnv,
    expect: &Expectation,
  ) -> InferTy {
    let ty = match &expr.kind {
      ExprKind::Literal(lit) => self.check_literal_with_span(lit, expr.span),

      ExprKind::Path(path) => {
        let explicit_args: Vec<InferTy> = path
          .segments
          .iter()
          .flat_map(|seg| seg.args.iter())
          .map(|ty| self.lower_hir_ty(ty))
          .collect();
        self.check_path(path.def_id, env, Some(&explicit_args))
      }

      ExprKind::Binary { op: _, lhs, rhs } => {
        let lhs_ty = self.check_expr(lhs, env, &Expectation::none());
        let rhs_ty = self.check_expr(rhs, env, &Expectation::none());
        let x = self.unify(&lhs_ty, &rhs_ty);
        self.handle_unify_result(x);
        lhs_ty
      }

      ExprKind::Unary { op: _, operand } => {
        // TODO: Implement unary operator type checking
        self.check_expr(operand, env, &Expectation::none())
      }

      ExprKind::Call { callee, args } => {
        self.check_call(callee, args, env, expr.span, expr.hir_id)
      }

      ExprKind::If { condition, then_block, else_branch } => {
        let cond_ty = self.check_expr(
          condition,
          env,
          &Expectation::has_type(InferTy::Primitive(PrimitiveKind::Bool, condition.span)),
        );
        self.unify(&cond_ty, &InferTy::Primitive(PrimitiveKind::Bool, condition.span));

        let then_ty = self.check_block(then_block, env, expect);

        if let Some(else_expr) = else_branch {
          let else_ty = self.check_expr(else_expr, env, expect);
          self.unify(&then_ty, &else_ty);
          then_ty
        } else {
          InferTy::Unit(expr.span)
        }
      }

      ExprKind::Block(block) => self.check_block(block, env, expect),

      ExprKind::Tuple(exprs) => {
        let tys: Vec<InferTy> =
          exprs.iter().map(|e| self.check_expr(e, env, &Expectation::none())).collect();
        InferTy::Tuple(tys, expr.span)
      }

      ExprKind::Array(exprs, _repeat) => {
        if exprs.is_empty() {
          InferTy::Array {
            ty: Box::new(self.infcx.fresh(expr.span)),
            len: 0,
            span: expr.span,
          }
        } else {
          let elem_ty = self.check_expr(&exprs[0], env, &Expectation::none());
          for e in exprs.iter().skip(1) {
            let ty = self.check_expr(e, env, &Expectation::has_type(elem_ty.clone()));
            self.unify(&ty, &elem_ty);
          }
          InferTy::Array { ty: Box::new(elem_ty), len: exprs.len(), span: expr.span }
        }
      }

      ExprKind::Index { object, index } => {
        let obj_ty = self.check_expr(object, env, &Expectation::none());
        let idx_ty = self.check_expr(index, env, &Expectation::none());

        // Index must be usize until we support operator overloading
        self.unify(&idx_ty, &InferTy::Primitive(PrimitiveKind::USize, index.span));

        match self.infcx.resolve(&obj_ty) {
          InferTy::Array { ty, .. } => *ty,
          InferTy::Slice(ty, _span) => *ty,
          _ => {
            // TODO: Report error - not indexable
            InferTy::Err(expr.span)
          }
        }
      }

      ExprKind::Field { object, field } => {
        let obj_ty = self.check_expr(object, env, &Expectation::none());
        self.check_field_access(&obj_ty, field, object.span)
      }

      ExprKind::Assign { target, value, .. } => {
        let target_ty = self.check_expr(target, env, &Expectation::none());
        let value_ty =
          self.check_expr(value, env, &Expectation::has_type(target_ty.clone()));
        self.unify(&value_ty, &target_ty);
        InferTy::Unit(expr.span)
      }

      ExprKind::Return { value } => {
        if let Some(val) = value {
          self.check_expr(val, env, expect);
        }
        InferTy::Never(expr.span)
      }

      ExprKind::Break { value, .. } => {
        if let Some(val) = value {
          self.check_expr(val, env, &Expectation::none());
        }
        InferTy::Never(expr.span)
      }

      ExprKind::Continue => InferTy::Never(expr.span),

      ExprKind::Struct { def_id, fields } => {
        self.check_struct_init(def_id, fields, env, expr)
      }

      ExprKind::Comptime { args, .. } => {
        let builtin = self
          .resolver
          .get_recorded_comptime_builtin(self.hir.hir_to_node(&expr.hir_id).unwrap());

        if let Some(builtin) = builtin {
          let func = get_builtin(&builtin);

          if func.params.len() != args.len() {
            self.dcx().emit(ArityMismatch {
              span: expr.span,
              callee: format!("builtin function `{}!`", builtin.name()),
              expected: func.params.len(),
              found: args.len(),
            });

            return InferTy::Err(expr.span);
          }

          let mut final_args = vec![];
          for (param, arg) in func.params.iter().zip(args) {
            match param {
              BuiltInParam::Type => {
                if let ComptimeArg::Expr(expr) = arg {
                  self.dcx().emit(FuncArgMismatch {
                    span: expr.span,
                    expected: "a comptime type".to_string(),
                    found: "an expression".to_string(),
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

                  match &result {
                    UnifyResult::Err(err) => {
                      if let UnifyError::Mismatch { .. } = err {
                        self.dcx().emit(FuncArgMismatch {
                          span: expr.span,
                          expected: self.format_type(ty),
                          found: self.format_type(&expr_ty),
                        });
                      } else {
                        self.handle_unify_result(result);
                      }
                    }
                    UnifyResult::Ok => {
                      final_args.push(FinalComptimeArg::Expr(*expr.clone()));
                    }
                  }
                }
              }
            }
          }

          self.table.comptime_args.insert(expr.hir_id, final_args);
        }

        InferTy::Err(expr.span)
      }

      _ => {
        warn!("not handled in type checker {expr:#?}");
        // TODO: Handle remaining expression kinds
        self.infcx.fresh(expr.span)
      }
    };

    self.table.record_node_type(expr.hir_id, ty.clone());
    ty
  }

  pub(crate) fn check_literal_with_span(&self, lit: &Literal, span: Span) -> InferTy {
    match lit {
      Literal::Int { suffix, .. } => {
        if let Some(suffix) = suffix {
          InferTy::Primitive(suffix.to_primitive_kind(), span)
        } else {
          self.infcx.fresh_int(span)
        }
      }
      Literal::Float { suffix, .. } => {
        if let Some(suffix) = suffix {
          InferTy::Primitive(suffix.to_primitive_kind(), span)
        } else {
          self.infcx.fresh_float(span)
        }
      }
      Literal::Bool(_) => InferTy::Primitive(PrimitiveKind::Bool, span),
      Literal::Char(_) => InferTy::Primitive(PrimitiveKind::Char, span),
      Literal::String(_) => InferTy::Ptr {
        mutability: Mutability::Const,
        ty: Box::new(InferTy::Primitive(PrimitiveKind::U8, span)),
        span,
      },
      Literal::Unit => InferTy::Unit(span),
    }
  }
}
