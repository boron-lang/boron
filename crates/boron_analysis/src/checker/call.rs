use crate::checker::TyChecker;
use crate::errors::{ArityMismatch, FuncArgMismatch};
use crate::table::TypeEnv;
use crate::ty::{InferTy, SubstitutionMap, TyParam};
use crate::unify::{Expectation, UnifyError, UnifyResult};
use boron_hir::{Expr, ExprKind, HirId};
use boron_session::prelude::Span;

impl TyChecker<'_> {
  pub fn check_call(
    &mut self,
    callee: &Expr,
    args: &[Expr],
    env: &mut TypeEnv,
    span: Span,
    call_hir_id: HirId,
  ) -> InferTy {
    let (callee_def_id, explicit_type_args) = match &callee.kind {
      ExprKind::Path(path) => {
        let args: Vec<InferTy> = path
          .segments
          .iter()
          .flat_map(|seg| seg.args.iter())
          .map(|ty| self.lower_hir_ty(ty))
          .collect();
        (Some(path.def_id), args)
      }
      _ => (None, vec![]),
    };

    let callee_ty = self.check_expr(callee, env, &Expectation::none());
    let resolved = self.infcx.resolve(&callee_ty);

    match resolved {
      InferTy::Fn { params, ret, span: _fn_span } => {
        let def = self.resolver.get_definition(callee_def_id.unwrap()).unwrap();

        if args.len() != params.len() {
          self.dcx().emit(ArityMismatch {
            callee: format!("function `{}`", def.name),
            span: callee.span,
            expected: params.len(),
            found: args.len(),
          });
        }
        for (arg, param_ty) in args.iter().zip(params.iter()) {
          let arg_ty =
            self.check_expr(arg, env, &Expectation::has_type(param_ty.clone()));
          let result = self.unify(param_ty, &arg_ty);

          if let UnifyResult::Err(err) = &result {
            match err {
              UnifyError::Mismatch { .. } => {
                self.dcx().emit(FuncArgMismatch {
                  span: arg.span,
                  expected: self.format_type(param_ty),
                  found: self.format_type(&arg_ty),
                });
              }
              _ => self.handle_unify_result(result, arg.span),
            }
          }
        }

        if let Some(def_id) = callee_def_id {
          if let Some(scheme) = self.table.def_type(def_id) {
            let mut subst = SubstitutionMap::new();
            if !scheme.vars.is_empty() {
              if !explicit_type_args.is_empty()
                && explicit_type_args.len() == scheme.vars.len()
              {
                for (var, arg) in scheme.vars.iter().zip(explicit_type_args.iter()) {
                  subst.add(*var, self.infcx.resolve(arg));
                }
              } else if let InferTy::Fn { params: inst_params, ret: inst_ret, .. } =
                &callee_ty
              {
                if let InferTy::Fn { params: scheme_params, ret: scheme_ret, .. } =
                  &scheme.ty
                {
                  for (scheme_param, inst_param) in
                    scheme_params.iter().zip(inst_params.iter())
                  {
                    Self::collect_param_substitutions(
                      scheme_param,
                      &self.infcx.resolve(inst_param),
                      &scheme.vars,
                      &mut subst,
                    );
                  }
                  Self::collect_param_substitutions(
                    scheme_ret,
                    &self.infcx.resolve(inst_ret),
                    &scheme.vars,
                    &mut subst,
                  );
                }
              }
              self.table.record_monomorphization(def_id, subst.clone());
            }

            self.table.record_expr_monomorphization(call_hir_id, def_id, subst);
          }
        }

        *ret
      }
      InferTy::Var(_, _var_span) => {
        let arg_tys: Vec<InferTy> =
          args.iter().map(|a| self.check_expr(a, env, &Expectation::none())).collect();
        let ret_ty = self.infcx.fresh(span);
        let expected_fn =
          InferTy::Fn { params: arg_tys, ret: Box::new(ret_ty.clone()), span };
        let result = self.unify(&callee_ty, &expected_fn);
        self.handle_unify_result(result, span);
        ret_ty
      }
      _ => {
        // TODO: Report error - not callable
        InferTy::Err(span)
      }
    }
  }

  pub fn collect_param_substitutions(
    scheme_ty: &InferTy,
    resolved_ty: &InferTy,
    vars: &[TyParam],
    subst: &mut SubstitutionMap,
  ) {
    match (scheme_ty, resolved_ty) {
      (InferTy::Param(param), resolved) => {
        if vars.iter().any(|v| v.def_id == param.def_id) {
          subst.add(*param, resolved.clone());
        }
      }
      (InferTy::Ptr { ty: s_ty, .. }, InferTy::Ptr { ty: r_ty, .. })
      | (InferTy::Optional(s_ty, _), InferTy::Optional(r_ty, _))
      | (InferTy::Array { ty: s_ty, .. }, InferTy::Array { ty: r_ty, .. })
      | (InferTy::Slice(s_ty, _), InferTy::Slice(r_ty, _)) => {
        Self::collect_param_substitutions(s_ty, r_ty, vars, subst);
      }
      (InferTy::Tuple(s_tys, _), InferTy::Tuple(r_tys, _)) => {
        for (s, r) in s_tys.iter().zip(r_tys.iter()) {
          Self::collect_param_substitutions(s, r, vars, subst);
        }
      }
      (
        InferTy::Fn { params: s_params, ret: s_ret, .. },
        InferTy::Fn { params: r_params, ret: r_ret, .. },
      ) => {
        for (s, r) in s_params.iter().zip(r_params.iter()) {
          Self::collect_param_substitutions(s, r, vars, subst);
        }
        Self::collect_param_substitutions(s_ret, r_ret, vars, subst);
      }
      (InferTy::Adt { args: s_args, .. }, InferTy::Adt { args: r_args, .. }) => {
        for (s, r) in s_args.iter().zip(r_args.iter()) {
          Self::collect_param_substitutions(s, r, vars, subst);
        }
      }
      _ => {}
    }
  }
}
