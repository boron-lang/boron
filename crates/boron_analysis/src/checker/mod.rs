mod call;
mod debug;
mod expr;
mod field;
mod path;
mod pattern;
mod stmt;
mod struct_init;

use crate::errors::{ConstInitMismatch, ReturnTypeMismatch};
use crate::interpreter::Interpreter;
use crate::interpreter::{InterpreterCache, InterpreterContext, InterpreterMode};
use crate::table::{InferCtx, TypeEnv, TypeTable};
use crate::ty::InferTy;
use crate::unify::Expectation;
use crate::unify::{UnifyError, UnifyResult};
use boron_diagnostics::DiagnosticCtx;
use boron_hir::{Const, Function, Hir, ParamKind};
use boron_resolver::{DefId, Resolver};
use boron_utils::context::Context;
use boron_utils::prelude::Span;

pub fn typeck_hir(hir: &Hir, ctx: &Context<'_>, resolver: &Resolver) -> TypeTable {
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

  pub(crate) fn handle_unify_result(&self, result: UnifyResult, span: Span) {
    match result {
      UnifyResult::Ok => {}
      UnifyResult::Err(err) => self.emit_unify_error(err, span),
    }
  }

  fn emit_unify_error(&self, err: UnifyError, span: Span) {
    use crate::errors::*;
    match err {
      UnifyError::Mismatch { expected, found } => {
        self.dcx().emit(TypeMismatch {
          expected_span: expected.span(),
          expected: self.format_type(&expected),
          found_span: found.span(),
          found: self.format_type(&found),
        });
      }
      UnifyError::OccursCheck { var, ty } => {
        self.dcx().emit(OccursCheck {
          var_span: span,
          var,
          ty_span: ty.span(),
          ty: self.format_type(&ty),
        });
      }
      UnifyError::ArityMismatch { expected, found } => {
        self.dcx().emit(TupleArityMismatch { span, expected, found });
      }
      UnifyError::NotAnInteger { ty } => {
        self.dcx().emit(NotAnInteger { span: ty.span(), found: self.format_type(&ty) });
      }
      UnifyError::NotAFloat { ty } => {
        self.dcx().emit(NotAFloat { span: ty.span(), found: self.format_type(&ty) });
      }
      UnifyError::MutabilityMismatch { expected, found } => {
        self.dcx().emit(MutabilityMismatch {
          span,
          expected: format!("{expected:?}").to_lowercase(),
          found: format!("{found:?}").to_lowercase(),
        });
      }
      UnifyError::ArrayLenMismatch { expected, found } => {
        self.dcx().emit(ArrayLenMismatch { span, expected, found });
      }
      UnifyError::IncompatibleKinds { kind1, kind2 } => {
        self.dcx().emit(IncompatibleKinds {
          span,
          kind1: format!("{kind1:?}"),
          kind2: format!("{kind2:?}"),
        });
      }
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
      let (body_ty, ret_span) =
        self.check_block(body, &mut env, &Expectation::has_type(expected_ret.clone()));

      let result = self.unify(&body_ty, &expected_ret);
      if let UnifyResult::Err(err) = &result {
        match err {
          UnifyError::Mismatch { .. } => {
            self.dcx().emit(ReturnTypeMismatch {
              expected_span: func.return_type.span,
              expected: self.format_type(&expected_ret),
              body_span: ret_span,
              found: self.format_type(&body_ty),
            });
          }
          _ => self.handle_unify_result(result, body.span),
        }
      }
    }
  }

  fn typeck_const(&mut self, _def_id: DefId, konst: &Const) {
    let mut env = TypeEnv::new();
    let expected = self.lower_hir_ty(&konst.ty);

    let init_ty =
      self.check_expr(&konst.value, &mut env, &Expectation::has_type(expected.clone()));
    let result = self.unify(&init_ty, &expected);
    if let UnifyResult::Err(err) = &result {
      match err {
        UnifyError::Mismatch { expected, found } => {
          self.dcx().emit(ConstInitMismatch {
            expected_span: konst.ty.span,
            expected: self.format_type(expected),
            value_span: konst.value.span,
            found: self.format_type(found),
          });
        }
        _ => self.handle_unify_result(result, konst.value.span),
      }
    }
  }
}
