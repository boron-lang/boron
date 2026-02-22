use crate::checker::TyChecker;
use crate::errors::{
  AssignTypeMismatch, IndexTypeMismatch, InvalidBinaryOp, TypeMismatch,
};
use crate::table::TypeEnv;
use crate::ty::{ArrayLength, InferTy};
use crate::unify::{Expectation, UnifyError, UnifyResult};
use boron_hir::expr::{ElseBranch, IfExpr};
use boron_hir::{Block, Expr, ExprKind, Literal};
use boron_parser::ast::types::PrimitiveKind;
use boron_parser::{BinaryOp, Mutability};
use boron_session::prelude::{Span, warn};

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

      ExprKind::Binary { op, lhs, rhs } => {
        let lhs_ty = self.check_expr(lhs, env, &Expectation::none());
        let rhs_ty = self.check_expr(rhs, env, &Expectation::none());
        let lhs_resolved = self.infcx.resolve(&lhs_ty);
        let rhs_resolved = self.infcx.resolve(&rhs_ty);

        let invalid = || {
          self.dcx().emit(InvalidBinaryOp {
            op: op.to_string(),
            lhs: self.format_type(&lhs_resolved),
            rhs: self.format_type(&rhs_resolved),
            span: expr.span,
          });
          InferTy::Err(expr.span)
        };

        match op {
          BinaryOp::Eq
          | BinaryOp::Ne
          | BinaryOp::Lt
          | BinaryOp::Le
          | BinaryOp::Gt
          | BinaryOp::Ge => {
            let result = self.unify(&lhs_ty, &rhs_ty);

            if let UnifyResult::Err(err) = &result
              && let UnifyError::Mismatch { .. } = err
            {
              invalid()
            } else {
              self.handle_unify_result(result, expr.span);
              InferTy::Primitive(PrimitiveKind::Bool, expr.span)
            }
          }

          BinaryOp::And | BinaryOp::Or => {
            let expected = InferTy::Primitive(PrimitiveKind::Bool, expr.span);
            let left = self.unify(&lhs_ty, &expected);
            let right = self.unify(&rhs_ty, &expected);

            if matches!(left, UnifyResult::Err(UnifyError::Mismatch { .. }))
              || matches!(right, UnifyResult::Err(UnifyError::Mismatch { .. }))
            {
              invalid()
            } else {
              self.handle_unify_result(left, expr.span);
              self.handle_unify_result(right, expr.span);
              expected
            }
          }

          BinaryOp::BitAnd
          | BinaryOp::BitOr
          | BinaryOp::BitXor
          | BinaryOp::Shl
          | BinaryOp::Shr => {
            if !self.is_int(&lhs_resolved) || !self.is_int(&rhs_resolved) {
              invalid()
            } else {
              let result = self.unify(&lhs_ty, &rhs_ty);
              if let UnifyResult::Err(err) = &result
                && let UnifyError::Mismatch { .. } = err
              {
                invalid()
              } else {
                self.handle_unify_result(result, expr.span);
                lhs_ty
              }
            }
          }

          BinaryOp::Add
          | BinaryOp::Sub
          | BinaryOp::Mul
          | BinaryOp::Div
          | BinaryOp::Mod => {
            if !self.is_numeric(&lhs_resolved) || !self.is_numeric(&rhs_resolved) {
              invalid()
            } else {
              let result = self.unify(&lhs_ty, &rhs_ty);
              if let UnifyResult::Err(err) = &result
                && let UnifyError::Mismatch { .. } = err
              {
                invalid()
              } else {
                self.handle_unify_result(result, expr.span);
                lhs_ty
              }
            }
          }
        }
      }

      ExprKind::Unary { op, operand } => self.check_unary(env, expr, op, operand),

      ExprKind::Call { callee, args } => {
        self.check_call(callee, args, env, expr.span, expr.hir_id)
      }

      ExprKind::If(IfExpr { condition, then_block, else_branch, .. }) => {
        self.check_if(condition, then_block, else_branch, env, expect)
      }

      ExprKind::Block(block) => self.check_block(block, env, expect).0,

      ExprKind::Tuple(exprs) => {
        let tys: Vec<InferTy> =
          exprs.iter().map(|e| self.check_expr(e, env, &Expectation::none())).collect();
        InferTy::Tuple(tys, expr.span)
      }

      ExprKind::Array(exprs, repeat) => {
        if exprs.is_empty() {
          InferTy::Array {
            ty: Box::new(self.infcx.fresh(expr.span)),
            len: ArrayLength::Len(0),
            span: expr.span,
          }
        } else {
          let elem_ty = self.check_expr(&exprs[0], env, &Expectation::none());
          for e in exprs.iter().skip(1) {
            let ty = self.check_expr(e, env, &Expectation::has_type(elem_ty.clone()));
            let result = self.unify(&ty, &elem_ty);
            self.handle_unify_result(result, e.span);
          }

          if let Some(repeat) = repeat {
            let len = self.interpret_array_len(repeat);
            return InferTy::Array { ty: Box::new(elem_ty), len, span: expr.span };
          }

          InferTy::Array {
            ty: Box::new(elem_ty),
            len: ArrayLength::Len(exprs.len()),
            span: expr.span,
          }
        }
      }

      ExprKind::Index { object, index } => {
        let obj_ty = self.check_expr(object, env, &Expectation::none());
        let idx_ty = self.check_expr(index, env, &Expectation::none());

        // Index must be usize until we support operator overloading
        let result =
          self.unify(&idx_ty, &InferTy::Primitive(PrimitiveKind::USize, index.span));
        if let UnifyResult::Err(err) = &result {
          match err {
            UnifyError::Mismatch { .. } => {
              self.dcx().emit(IndexTypeMismatch {
                span: index.span,
                found: self.format_type(&idx_ty),
              });
            }
            _ => self.handle_unify_result(result, index.span),
          }
        }

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
        self.check_field_access(&obj_ty, field)
      }

      ExprKind::Assign { target, value, .. } => {
        let target_ty = self.check_expr(target, env, &Expectation::none());
        let value_ty =
          self.check_expr(value, env, &Expectation::has_type(target_ty.clone()));
        let result = self.unify(&value_ty, &target_ty);
        if let UnifyResult::Err(err) = &result {
          match err {
            UnifyError::Mismatch { expected, found } => {
              self.dcx().emit(AssignTypeMismatch {
                target_span: target.span,
                expected: self.format_type(expected),
                value_span: value.span,
                found: self.format_type(found),
              });
            }
            _ => self.handle_unify_result(result, value.span),
          }
        }
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

      ExprKind::MethodCall { receiver, method, args } => {
        self.check_method_call(receiver, method, args, env, expr.span, expr.hir_id)
      }

      ExprKind::Struct { def_id, fields } => {
        self.check_struct_init(def_id, fields, env, expr)
      }

      ExprKind::Comptime { args, callee } => self.check_comptime(expr, env, callee, args),

      _ => {
        warn!("not handled in type checker {expr:#?}");
        // TODO: Handle remaining expression kinds
        self.infcx.fresh(expr.span)
      }
    };

    self.table.record_node_type(expr.hir_id, ty.clone());
    ty
  }

  pub fn check_if(
    &mut self,
    condition: &Expr,
    then_block: &Block,
    else_branch: &Option<ElseBranch>,
    env: &mut TypeEnv,
    expect: &Expectation,
  ) -> InferTy {
    let cond_ty = self.check_expr(
      condition,
      env,
      &Expectation::has_type(InferTy::Primitive(PrimitiveKind::Bool, condition.span)),
    );

    let cond_result =
      self.unify(&cond_ty, &InferTy::Primitive(PrimitiveKind::Bool, condition.span));
    self.handle_unify_result(cond_result, condition.span);

    let (then_ty, then_span) = self.check_block(then_block, env, expect);

    if let Some(else_expr) = else_branch {
      match else_expr {
        ElseBranch::Block(block) => {
          let (else_ty, found_span) = self.check_block(block, env, expect);
          let result = self.unify(&then_ty, &else_ty);
          if let UnifyResult::Err(err) = &result {
            match err {
              UnifyError::Mismatch { .. } => {
                self.dcx().emit(TypeMismatch {
                  expected_span: then_span,
                  expected: self.format_type(&then_ty),
                  found_span,
                  found: self.format_type(&else_ty),
                });
              }
              _ => self.handle_unify_result(result, found_span),
            }
          }

          self.table.record_node_type(block.hir_id, then_ty.clone());
          then_ty
        }
        ElseBranch::If(expr) => {
          let ty = self.check_if(
            &expr.condition,
            &expr.then_block,
            &expr.else_branch,
            env,
            expect,
          );
          self.table.record_node_type(expr.id, ty.clone());
          ty
        }
      }
    } else {
      InferTy::Unit(condition.span)
    }
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
