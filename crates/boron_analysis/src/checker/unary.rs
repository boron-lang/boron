use crate::errors::{InvalidUnaryOp, TyCantBeDereferenced, UnaryNotOnNumeric};
use crate::unify::{Expectation, UnifyError, UnifyResult};
use crate::{InferTy, TyChecker, TypeEnv};
use boron_hir::Expr;
use boron_parser::{PrimitiveKind, UnaryOp};

impl<'a> TyChecker<'a> {
  pub fn check_unary(
    &mut self,
    env: &mut TypeEnv,
    expr: &Expr,
    op: &UnaryOp,
    operand: &Expr,
  ) -> InferTy {
    let operand_ty = self.check_expr(operand, env, &Expectation::none());
    let resolved = self.infcx.resolve(&operand_ty);

    let invalid = || {
      self.dcx().emit(InvalidUnaryOp {
        op: op.to_string(),
        ty: self.format_type(&resolved),
        span: expr.span,
      });
      InferTy::Err(expr.span)
    };

    match op {
      UnaryOp::Not => {
        if self.is_numeric(&resolved) {
          self.dcx().emit(UnaryNotOnNumeric { span: expr.span });

          return InferTy::Err(expr.span);
        }

        let expected = InferTy::Primitive(PrimitiveKind::Bool, expr.span);
        match self.unify(&operand_ty, &expected) {
          UnifyResult::Err(UnifyError::Mismatch { .. }) => invalid(),
          result => {
            self.handle_unify_result(result, expr.span);
            expected
          }
        }
      }

      UnaryOp::BitNot => {
        let expected = self.infcx.fresh_int(expr.span);
        if !self.is_int(&resolved) {
          return invalid();
        }

        match self.unify(&operand_ty, &expected) {
          UnifyResult::Err(UnifyError::Mismatch { .. }) => invalid(),
          result => {
            self.handle_unify_result(result, expr.span);
            expected
          }
        }
      }

      UnaryOp::Neg | UnaryOp::Plus => {
        if !self.is_numeric(&resolved) {
          invalid()
        } else {
          operand_ty
        }
      }

      UnaryOp::Deref => match &resolved {
        InferTy::Ptr { ty, .. } => ty.as_ref().clone(),
        _ => {
          self.dcx().emit(TyCantBeDereferenced {
            span: expr.span,
            ty: self.format_type(&operand_ty),
          });

          InferTy::Err(expr.span)
        }
      },
    }
  }
}
