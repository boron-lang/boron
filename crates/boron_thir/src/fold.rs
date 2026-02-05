use crate::ThirLowerer;
use boron_analysis::interpreter::values::ConstValue;
use boron_analysis::literal_table::FullLiteral;
use boron_hir::{expr::Expr as HirExpr, expr::ExprKind as HirExprKind};
use boron_parser::UnaryOp;

impl<'a> ThirLowerer<'a> {
  pub fn can_const_fold(&self, expr: &HirExpr) -> bool {
    match &expr.kind {
      HirExprKind::Literal(_) => true,
      HirExprKind::Path(path) => self.hir.get_const(path.def_id).is_some(),
      HirExprKind::Binary { lhs, rhs, .. } => {
        self.can_const_fold(lhs) && self.can_const_fold(rhs)
      }
      HirExprKind::Unary { operand, op } => {
        !matches!(op, UnaryOp::Deref) && self.can_const_fold(operand)
      }
      HirExprKind::Tuple(exprs) => exprs.iter().all(|e| self.can_const_fold(e)),
      HirExprKind::Array(exprs, len) => {
        exprs.iter().all(|e| self.can_const_fold(e))
          && len.as_ref().is_none_or(|l| self.can_const_fold(l))
      }
      _ => false,
    }
  }

  pub fn try_const_fold(&'a self, expr: &HirExpr) -> Option<FullLiteral> {
    if !self.can_const_fold(expr) {
      return None;
    }

    let interpreter = self.new_interpreter();
    let value = interpreter.evaluate_expr(expr);

    Self::const_value_to_literal(value)
  }

  pub fn const_value_to_literal(value: ConstValue) -> Option<FullLiteral> {
    match value {
      ConstValue::Int(i) => Some(FullLiteral::Int(i)),
      ConstValue::Bool(b) => Some(FullLiteral::Bool(b)),
      ConstValue::Char(c) => Some(FullLiteral::Char(c)),
      ConstValue::String(s) => Some(FullLiteral::String(s)),
      ConstValue::Float(f) => Some(FullLiteral::Float(f)),
      ConstValue::Unit => Some(FullLiteral::Unit),

      ConstValue::Array(_)
      | ConstValue::Struct(_)
      | ConstValue::Enum { .. }
      | ConstValue::Type(_)
      | ConstValue::Poison => None,
    }
  }
}
