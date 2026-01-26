mod size_of;

use crate::InferTy;
use zirael_hir::Expr;

#[derive(Debug)]
pub enum FinalComptimeArg {
  Ty(InferTy),
  Expr(Expr),
}

impl FinalComptimeArg {
  pub fn as_ty(&self) -> InferTy {
    match self {
      Self::Ty(ty) => ty.clone(),
      _ => unreachable!(),
    }
  }

  pub fn as_expr(&self) -> Expr {
    match self {
      Self::Expr(expr) => expr.clone(),
      _ => unreachable!(),
    }
  }
}
