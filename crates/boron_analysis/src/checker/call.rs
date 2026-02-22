use crate::TypeScheme;
use crate::checker::TyChecker;
use crate::errors::{
  ArityMismatch, CannotCall, CannotConstructEnumVariantUsingCall, FuncArgMismatch,
  NoMethodForTy, NoValuePassedForParameter,
};
use crate::table::TypeEnv;
use crate::ty::{InferTy, SubstitutionMap, TyParam};
use crate::unify::{Expectation, UnifyError, UnifyResult};
use boron_hir::expr::Argument;
use boron_hir::item::VariantKind;
use boron_hir::{Expr, ExprKind, HirId};
use boron_resolver::DefId;
use boron_session::prelude::{Span, debug};
use boron_source::ident_table::Identifier;
use itertools::Itertools as _;
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
    let (callee_def_id, explicit_type_args, last_segment) =
      self.resolve_callee_metadata(callee, env);
    let Some(def_id) = callee_def_id else { panic!() };

    let callee_ty = self.check_expr(callee, env, &Expectation::none());
    let resolved = self.infcx.resolve(&callee_ty);

    match &resolved {
      InferTy::Fn { params, ret, .. } => {
        self.check_fn_call(callee, args, params, env, def_id);

        self.handle_monomorphization(
          callee_def_id,
          &callee_ty,
          &explicit_type_args,
          call_hir_id,
        );

        *ret.clone()
      }

      InferTy::Adt { def_id, .. } => {
        let enum_ = self.hir.get_enum(*def_id).expect("should exist");
        let variant = enum_
          .variants
          .iter()
          .find(|variant| variant.name == last_segment)
          .expect("checked in resolver");

        let scheme = self.table.def_type(*def_id).unwrap();
        let (resolved, subst) = self.instantiate(&scheme);

        let VariantKind::Tuple(tuple) = &variant.kind else {
          self.dcx().emit(CannotConstructEnumVariantUsingCall {
            span: callee.span,
            name: last_segment,
          });
          return InferTy::Err(Span::dummy());
        };

        if args.len() != tuple.len() {
          self.dcx().emit(ArityMismatch {
            callee: format!("tuple enum variant {last_segment}"),
            span: callee.span,
            expected: tuple.len(),
            found: args.len(),
          });
        }

        for (i, arg) in args.iter().enumerate() {
          let raw_ty = self.lower_hir_ty(&tuple[i]);
          let expected_ty = Self::apply_subst(&raw_ty, &subst);
          let arg_ty =
            self.check_expr(&arg.value, env, &Expectation::has_type(expected_ty.clone()));
          self.check_arg_unification(&arg.value, &expected_ty, &arg_ty);
        }

        let resolved = Self::apply_subst(&resolved, &subst);
        self.table.record_monomorphization(*def_id, subst.clone());
        self.table.record_expr_monomorphization(call_hir_id, *def_id, subst);

        resolved
      }

      InferTy::Var(_, _) => self.infer_call_from_var(callee_ty, args, env, span),

      _ => {
        debug!("cannot call on {resolved:#?}");
        self
          .dcx()
          .emit(CannotCall { span: callee.span, callee: self.format_type(&resolved) });
        InferTy::Err(span)
      }
    }
  }

  fn resolve_callee_metadata(
    &mut self,
    callee: &Expr,
    env: &mut TypeEnv,
  ) -> (Option<DefId>, Vec<InferTy>, Identifier) {
    if let ExprKind::Path(path) = &callee.kind {
      let args = path
        .segments
        .iter()
        .flat_map(|seg| seg.args.iter())
        .map(|ty| self.lower_hir_ty(ty))
        .collect();

      (Some(path.def_id), args, path.segments.last().expect("no last segment").name)
    } else {
      debug!("calling on {callee:#?}");
      (None, vec![], Identifier::dummy())
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
        });
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
    let Some(def_id) = callee_def_id else { panic!("do def id") };
    let Some(scheme) = self.table.def_type(def_id) else { panic!("no ty scheme") };

    let mut subst = SubstitutionMap::new();

    if !scheme.vars.is_empty() {
      if self.apply_explicit_type_args(&scheme, explicit_type_args, &mut subst) {
      } else {
        self.infer_substitutions_from_instantiation(&scheme, callee_ty, &mut subst);
      }

      self.table.record_monomorphization(def_id, subst.clone());
      self.monomorphize_parent(def_id, &mut subst);
    }

    self.table.record_expr_monomorphization(call_hir_id, def_id, subst);
  }

  fn monomorphize_parent(&mut self, def_id: DefId, subst: &mut SubstitutionMap) {
    if let Some(parent_struct_id) = self.resolver.find_parent(def_id) {
      if let Some(struct_scheme) = self.table.def_type(parent_struct_id) {
        if !struct_scheme.vars.is_empty() {
          let mut struct_subst = SubstitutionMap::new();
          for param in &struct_scheme.vars {
            if let Some(ty) = subst.get(param.def_id) {
              struct_subst.add(*param, ty.clone());
            }
          }
          self.table.record_monomorphization(parent_struct_id, struct_subst);
        }
      }
    }
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

  pub fn check_method_call(
    &mut self,
    receiver: &Expr,
    method: &Identifier,
    args: &[Argument],
    env: &mut TypeEnv,
    span: Span,
    call_hir_id: HirId,
  ) -> InferTy {
    let receiver_ty = self.check_expr(receiver, env, &Expectation::none());
    let resolved_receiver = self.infcx.resolve(&receiver_ty);

    let struct_def_id = if let InferTy::Adt { def_id, .. } = &resolved_receiver {
      *def_id
    } else {
      self.dcx().emit(CannotCall {
        span,
        callee: format!(
          "method `{}` on {}",
          method.text(),
          self.format_type(&resolved_receiver)
        ),
      });
      return InferTy::Err(span);
    };

    let Some(method_def_id) = self.resolver.lookup_adt_member(struct_def_id, method)
    else {
      self.dcx().emit(NoMethodForTy {
        span: *method.span(),
        ty: self.format_type(&resolved_receiver),
        method: *method,
      });
      return InferTy::Err(span);
    };

    let method_ty = self.check_path(method_def_id, env, None);
    let resolved_method = self.infcx.resolve(&method_ty);

    if let InferTy::Fn { params, ret, .. } = resolved_method {
      self.check_fn_call_args(args, &params, env, method_def_id, span);
      self.handle_monomorphization(Some(method_def_id), &method_ty, &[], call_hir_id);
      *ret
    } else {
      self.dcx().emit(CannotCall { span, callee: format!("method `{}`", method.text()) });
      InferTy::Err(span)
    }
  }

  fn check_fn_call_args(
    &mut self,
    args: &[Argument],
    param_tys: &[InferTy],
    env: &mut TypeEnv,
    def: DefId,
    span: Span,
  ) {
    let def_info = self.resolver.get_definition(def).expect("couldn't find def");

    if args.len() != param_tys.len() {
      self.dcx().emit(ArityMismatch {
        callee: format!("method {}", def_info.name),
        span,
        expected: param_tys.len(),
        found: args.len(),
      });
    }

    for (arg_idx, arg) in args.iter().enumerate() {
      if arg_idx >= param_tys.len() {
        break;
      }
      let expected_ty = &param_tys[arg_idx];
      let arg_ty =
        self.check_expr(&arg.value, env, &Expectation::has_type(expected_ty.clone()));
      self.check_arg_unification(&arg.value, expected_ty, &arg_ty);
    }
  }
}
