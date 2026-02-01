mod call;
mod debug;
mod expr;
mod field;
mod path;
mod pattern;
mod stmt;
mod struct_init;

use crate::interpreter::Interpreter;
use crate::interpreter::{InterpreterCache, InterpreterContext, InterpreterMode};
use crate::table::{InferCtx, TypeEnv, TypeTable};
use crate::unify::UnifyResult;
use boron_diagnostics::DiagnosticCtx;
use boron_hir::{Const, Function, Hir, ParamKind};
use boron_resolver::{DefId, Resolver};
use boron_utils::context::Context;
use boron_utils::prelude::warn;

use crate::ty::InferTy;
use crate::unify::Expectation;

pub fn typeck_hir(hir: &Hir, ctx: &Context, resolver: &Resolver) -> TypeTable {
  let mut checker = TyChecker::new(hir, ctx, resolver);

  checker.collect_signatures();

  for entry in &hir.functions {
    let def_id = *entry.key();
    let func = entry.value();
    checker.typeck_function(def_id, func);
  }

  for entry in &hir.consts {
    let def_id = *entry.key();
    let konst = entry.value();
    checker.typeck_const(def_id, konst);
  }

  checker.finalize_types();
  checker.debug_print_resolved_types();
  checker.table
}

pub struct TyChecker<'a> {
  pub hir: &'a Hir,
  pub ctx: &'a Context<'a>,
  pub resolver: &'a Resolver,
  pub table: TypeTable,
  pub infcx: InferCtx,
  pub interpreter_cache: InterpreterCache,
}

impl<'a> TyChecker<'a> {
  pub fn new(hir: &'a Hir, ctx: &'a Context<'a>, resolver: &'a Resolver) -> Self {
    Self {
      hir,
      ctx,
      resolver,
      table: TypeTable::new(),
      infcx: InferCtx::new(),
      interpreter_cache: InterpreterCache::new(),
    }
  }

  pub fn hir(&self) -> &'a Hir {
    self.hir
  }

  pub fn dcx(&self) -> &'a DiagnosticCtx {
    self.ctx.dcx()
  }

  pub fn new_interpreter(
    &'a self,
    mode: InterpreterMode,
    ctx: InterpreterContext,
  ) -> Interpreter<'a> {
    Interpreter::new(
      self.dcx(),
      &self.interpreter_cache,
      self.resolver,
      self.hir,
      mode,
      ctx,
    )
  }

  pub(crate) fn handle_unify_result(&self, result: UnifyResult) {
    match &result {
      UnifyResult::Ok => {}
      UnifyResult::Err(_err) => warn!("{result:#?}"),
    }
  }

  fn typeck_function(&mut self, _def_id: DefId, func: &Function) {
    let mut env = TypeEnv::new();

    for param in &func.params {
      let ty = match &param.kind {
        ParamKind::Regular { ty, .. } => {
          let param_ty = self.lower_hir_ty(ty);
          env.bind(param.def_id, param_ty.clone());
          param_ty
        }
        ParamKind::Variadic { ty, .. } => {
          let param_ty = InferTy::Slice(Box::new(self.lower_hir_ty(ty)), param.span);
          env.bind(param.def_id, param_ty.clone());
          param_ty
        }
        ParamKind::SelfParam { .. } => {
          // TODO: Bind self parameter
          todo!()
        }
      };

      self.table.record_node_type(param.hir_id, ty);
    }

    if let Some(body) = &func.body {
      let expected_ret = self.lower_hir_ty(&func.return_type);
      let body_ty =
        self.check_block(body, &mut env, &Expectation::has_type(expected_ret.clone()));

      self.unify(&body_ty, &expected_ret);
    }
  }

  fn typeck_const(&mut self, _def_id: DefId, konst: &Const) {
    let mut env = TypeEnv::new();
    let expected = self.lower_hir_ty(&konst.ty);

    let init_ty =
      self.check_expr(&konst.value, &mut env, &Expectation::has_type(expected.clone()));
    self.unify(&init_ty, &expected);
  }
}
