use crate::checker::TyChecker;
use crate::errors::{ArityMismatch, FuncArgMismatch, NoValuePassedForParameter};
use crate::table::TypeEnv;
use crate::ty::{InferTy, SubstitutionMap, TyParam};
use crate::unify::{Expectation, UnifyError, UnifyResult};
use crate::TypeScheme;
use boron_hir::expr::Argument;
use boron_hir::{Expr, ExprKind, HirId};
use boron_resolver::DefId;
use boron_session::prelude::Span;
use itertools::Itertools;
use std::collections::HashSet;

impl TyChecker<'_> {
  pub fn check_call(
    &mut self,
    callee: &Expr,
    args: &Vec<Argument>,
    env: &mut TypeEnv,
    span: Span,
    call_hir_id: HirId,
  ) -> InferTy {
    let (callee_def_id, explicit_type_args) = self.resolve_callee_metadata(callee);
    let Some(def_id) = callee_def_id else { panic!() };

    let callee_ty = self.check_expr(callee, env, &Expectation::none());
    let resolved = self.infcx.resolve(&callee_ty);

    match resolved {
      InferTy::Fn { params, ret, .. } => {
        self.check_fn_call(callee, args, &params, env, def_id);

        self.handle_monomorphization(
          callee_def_id,
          &callee_ty,
          &explicit_type_args,
          call_hir_id,
        );

        *ret
      }

      InferTy::Var(_, _) => self.infer_call_from_var(callee_ty, args, env, span),

      _ => InferTy::Err(span),
    }
  }

  fn resolve_callee_metadata(&mut self, callee: &Expr) -> (Option<DefId>, Vec<InferTy>) {
    match &callee.kind {
      ExprKind::Path(path) => {
        let args = path
          .segments
          .iter()
          .flat_map(|seg| seg.args.iter())
          .map(|ty| self.lower_hir_ty(ty))
          .collect();

        (Some(path.def_id), args)
      }
      _ => (None, vec![]),
    }
  }

  fn check_fn_call(
    &mut self,
    callee: &Expr,
    args: &Vec<Argument>,
    ty_params: &[InferTy],
    env: &mut TypeEnv,
    def: DefId,
  ) {
    let function = self.hir.get_function(def).expect("should exist");
    let def = self.resolver.get_definition(def).expect("couldn't find def");
    if args.len() != ty_params.len() {
      self.dcx().emit(ArityMismatch {
        callee: format!("function {}", def.name),
        span: callee.span,
        expected: ty_params.len(),
        found: args.len(),
      });
    }

    let mut set = HashSet::new();
    for (arg_idx, arg) in args.iter().enumerate() {
      let expected_ty = if let Some(name) = arg.name {
        let param = function.params.iter().find_position(|param| {
          let def =
            self.resolver.get_definition(param.def_id).expect("couldn't find parameter");

          def.name == name
        });

        let Some((index, p)) = param else { continue };
        set.insert(p.def_id);
        &ty_params[index]
      } else {
        let param_id = function.params[arg_idx].def_id;
        set.insert(param_id);
        &ty_params[arg_idx]
      };

      let arg_ty =
        self.check_expr(&arg.value, env, &Expectation::has_type(expected_ty.clone()));

      self.check_arg_unification(&arg.value, expected_ty, &arg_ty);
    }

    for param in &function.params {
      if set.get(&param.def_id).is_none() {
        let def = self.resolver.get_definition(param.def_id).unwrap();

        self.dcx().emit(NoValuePassedForParameter {
          param: def.name,
          func_call: callee.span,
          param_span: def.span,
        })
      }
    }
  }

  fn check_arg_unification(&mut self, arg: &Expr, expected: &InferTy, found: &InferTy) {
    let result = self.unify(expected, found);

    if let UnifyResult::Err(err) = &result {
      match err {
        UnifyError::Mismatch { .. } => {
          self.dcx().emit(FuncArgMismatch {
            span: arg.span,
            expected: self.format_type(expected),
            found: self.format_type(found),
          });
        }
        _ => self.handle_unify_result(result, arg.span),
      }
    }
  }

  fn infer_call_from_var(
    &mut self,
    callee_ty: InferTy,
    args: &Vec<Argument>,
    env: &mut TypeEnv,
    span: Span,
  ) -> InferTy {
    let arg_tys: Vec<InferTy> =
      args.iter().map(|a| self.check_expr(&a.value, env, &Expectation::none())).collect();

    let ret_ty = self.infcx.fresh(span);

    let expected_fn =
      InferTy::Fn { params: arg_tys, ret: Box::new(ret_ty.clone()), span };

    let result = self.unify(&callee_ty, &expected_fn);
    self.handle_unify_result(result, span);

    ret_ty
  }

  fn handle_monomorphization(
    &mut self,
    callee_def_id: Option<DefId>,
    callee_ty: &InferTy,
    explicit_type_args: &[InferTy],
    call_hir_id: HirId,
  ) {
    let Some(def_id) = callee_def_id else { return };
    let Some(scheme) = self.table.def_type(def_id) else { return };

    let mut subst = SubstitutionMap::new();

    if !scheme.vars.is_empty() {
      if self.apply_explicit_type_args(&scheme, explicit_type_args, &mut subst) {
      } else {
        self.infer_substitutions_from_instantiation(&scheme, callee_ty, &mut subst);
      }

      self.table.record_monomorphization(def_id, subst.clone());
    }

    self.table.record_expr_monomorphization(call_hir_id, def_id, subst);
  }

  fn apply_explicit_type_args(
    &mut self,
    scheme: &TypeScheme,
    explicit: &[InferTy],
    subst: &mut SubstitutionMap,
  ) -> bool {
    if explicit.len() != scheme.vars.len() {
      return false;
    }

    for (var, arg) in scheme.vars.iter().zip(explicit.iter()) {
      subst.add(*var, self.infcx.resolve(arg));
    }

    true
  }

  fn infer_substitutions_from_instantiation(
    &mut self,
    scheme: &TypeScheme,
    callee_ty: &InferTy,
    subst: &mut SubstitutionMap,
  ) {
    if let (
      InferTy::Fn { params: inst_params, ret: inst_ret, .. },
      InferTy::Fn { params: scheme_params, ret: scheme_ret, .. },
    ) = (callee_ty, &scheme.ty)
    {
      for (s, r) in scheme_params.iter().zip(inst_params.iter()) {
        Self::collect_param_substitutions(s, &self.infcx.resolve(r), &scheme.vars, subst);
      }

      Self::collect_param_substitutions(
        scheme_ret,
        &self.infcx.resolve(inst_ret),
        &scheme.vars,
        subst,
      );
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
