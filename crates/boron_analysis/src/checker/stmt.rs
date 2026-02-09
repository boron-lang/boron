use crate::checker::pattern::{ExpectedPattern, PatternContext};
use crate::checker::TyChecker;
use crate::errors::VarInitMismatch;
use crate::table::TypeEnv;
use crate::ty::InferTy;
use crate::unify::Expectation;
use crate::unify::{UnifyError, UnifyResult};
use boron_hir::{Block, ExprKind, Local, Stmt, StmtKind};
use boron_source::span::Span;

impl TyChecker<'_> {
  pub(crate) fn check_block(
    &mut self,
    block: &Block,
    env: &mut TypeEnv,
    expect: &Expectation,
  ) -> (InferTy, Span) {
    env.push_scope();

    let (mut last_ty, mut span) = (InferTy::Unit(block.span), Span::default());

    for stmt in &block.stmts {
      last_ty = self.check_stmt(stmt, env);
      span = stmt.span;
    }

    if let Some(expr) = &block.expr {
      last_ty = self.check_expr(expr, env, expect);
      span = expr.span;
    }

    env.pop_scope();
    (last_ty, span)
  }

  pub(crate) fn check_stmt(&mut self, stmt: &Stmt, env: &mut TypeEnv) -> InferTy {
    match &stmt.kind {
      StmtKind::Local(local) => self.check_local(env, local),
      StmtKind::Expr(expr) => {
        let ty = self.check_expr(expr, env, &Expectation::none());

        if let ExprKind::Return { .. } = &expr.kind {
          ty
        } else {
          InferTy::Unit(expr.span)
        }
      }
    }
  }

  pub fn check_local(&mut self, env: &mut TypeEnv, local: &Local) -> InferTy {
    let expected = match &local.ty {
      Some(ty) => self.lower_hir_ty(ty),
      None => self.infcx.fresh(local.span),
    };

    if let Some(init) = &local.init {
      let init_ty = self.check_expr(init, env, &Expectation::has_type(expected.clone()));
      let result = self.unify(&expected, &init_ty);

      if let UnifyResult::Err(err) = &result {
        match err {
          UnifyError::Mismatch { expected, found } => {
            self.dcx().emit(VarInitMismatch {
              expected: self.format_type(expected),
              found: self.format_type(found),
              span: local.span,
            });
          }
          _ => self.handle_unify_result(result, local.span),
        }
      }
    }

    self.check_pattern(&local.pat, &expected, env, ExpectedPattern::Irrefutable, PatternContext::Local);

    InferTy::Unit(local.span)
  }
}
