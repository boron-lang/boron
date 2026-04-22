use crate::hir::Expr;
use crate::infer_ty::InferTy;

#[derive(Debug, Clone)]
pub enum FinalComptimeArg {
  Ty(InferTy),
  Expr(Expr),
}

impl FinalComptimeArg {
  pub fn as_ty(&self) -> &InferTy {
    match self {
      Self::Ty(ty) => ty,
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
