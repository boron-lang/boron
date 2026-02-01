use crate::builtins::{BuiltInParam, get_builtin};
use crate::errors::{
  ArityMismatch, FieldInitMismatch, FuncArgMismatch, InvalidStructInit, NoFieldOnType,
  VarInitMismatch,
};
use crate::functions::FinalComptimeArg;
use crate::interpreter::{
  Interpreter, InterpreterCache, InterpreterContext, InterpreterMode,
};
use crate::table::{InferCtx, TypeEnv, TypeTable};
use crate::ty::{InferTy, SubstitutionMap, TyParam, TyVar, TypeScheme};
use crate::unify::{Expectation, UnifyError, UnifyResult};
use boron_diagnostics::DiagnosticCtx;
use boron_hir::expr::{ComptimeArg, FieldInit, PathExpr};
use boron_hir::{
  Block, Const, Expr, ExprKind, Function, Hir, HirId, Literal, ParamKind, Pat, PatKind,
  Stmt, StmtKind,
};
use boron_parser::Mutability;
use boron_parser::ast::types::PrimitiveKind;
use boron_resolver::prelude::BuiltInKind;
use boron_resolver::{DefId, DefKind, Resolver};
use boron_utils::context::Context;
use boron_utils::ident_table::Identifier;
use boron_utils::prelude::{Span, warn};
use std::collections::HashMap;

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

  fn handle_unify_result(&self, result: UnifyResult) {
    match &result {
      UnifyResult::Ok => {}
      UnifyResult::Err(err) => warn!("{result:#?}"),
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

  fn check_block(
    &mut self,
    block: &Block,
    env: &mut TypeEnv,
    expect: &Expectation,
  ) -> InferTy {
    env.push_scope();

    let mut last_ty = InferTy::Unit(block.span);

    for stmt in &block.stmts {
      last_ty = self.check_stmt(stmt, env);
    }

    if let Some(expr) = &block.expr {
      last_ty = self.check_expr(expr, env, expect);
    }

    env.pop_scope();
    last_ty
  }

  fn check_stmt(&mut self, stmt: &Stmt, env: &mut TypeEnv) -> InferTy {
    match &stmt.kind {
      StmtKind::Local(local) => {
        let expected = match &local.ty {
          Some(ty) => self.lower_hir_ty(ty),
          None => self.infcx.fresh(local.span),
        };

        if let Some(init) = &local.init {
          let init_ty =
            self.check_expr(init, env, &Expectation::has_type(expected.clone()));
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
              _ => self.handle_unify_result(result),
            }
          }
        }

        self.check_pattern(&local.pat, &expected, env);

        InferTy::Unit(local.span)
      }
      StmtKind::Expr(expr) => {
        self.check_expr(expr, env, &Expectation::none());
        InferTy::Unit(expr.span)
      }
      StmtKind::Semi(expr) => {
        self.check_expr(expr, env, &Expectation::none());
        InferTy::Unit(expr.span)
      }
    }
  }

  fn check_expr(
    &mut self,
    expr: &Expr,
    env: &mut TypeEnv,
    expect: &Expectation,
  ) -> InferTy {
    let ty = match &expr.kind {
      ExprKind::Literal(lit) => self.check_literal_with_span(lit, expr.span),

      ExprKind::Path(path) => {
        let explicit_args: Vec<InferTy> = path
          .segments
          .iter()
          .flat_map(|seg| seg.args.iter())
          .map(|ty| self.lower_hir_ty(ty))
          .collect();
        self.check_path(path.def_id, env, Some(&explicit_args))
      }

      ExprKind::Binary { op: _, lhs, rhs } => {
        let lhs_ty = self.check_expr(lhs, env, &Expectation::none());
        let rhs_ty = self.check_expr(rhs, env, &Expectation::none());
        let x = self.unify(&lhs_ty, &rhs_ty);
        self.handle_unify_result(x);
        lhs_ty
      }

      ExprKind::Unary { op: _, operand } => {
        // TODO: Implement unary operator type checking
        self.check_expr(operand, env, &Expectation::none())
      }

      ExprKind::Call { callee, args } => {
        self.check_call(callee, args, env, expr.span, expr.hir_id)
      }

      ExprKind::If { condition, then_block, else_branch } => {
        let cond_ty = self.check_expr(
          condition,
          env,
          &Expectation::has_type(InferTy::Primitive(PrimitiveKind::Bool, condition.span)),
        );
        self.unify(&cond_ty, &InferTy::Primitive(PrimitiveKind::Bool, condition.span));

        let then_ty = self.check_block(then_block, env, expect);

        if let Some(else_expr) = else_branch {
          let else_ty = self.check_expr(else_expr, env, expect);
          self.unify(&then_ty, &else_ty);
          then_ty
        } else {
          InferTy::Unit(expr.span)
        }
      }

      ExprKind::Block(block) => self.check_block(block, env, expect),

      ExprKind::Tuple(exprs) => {
        let tys: Vec<InferTy> =
          exprs.iter().map(|e| self.check_expr(e, env, &Expectation::none())).collect();
        InferTy::Tuple(tys, expr.span)
      }

      ExprKind::Array(exprs, repeat) => {
        if exprs.is_empty() {
          InferTy::Array {
            ty: Box::new(self.infcx.fresh(expr.span)),
            len: 0,
            span: expr.span,
          }
        } else {
          let elem_ty = self.check_expr(&exprs[0], env, &Expectation::none());
          for e in exprs.iter().skip(1) {
            let ty = self.check_expr(e, env, &Expectation::has_type(elem_ty.clone()));
            self.unify(&ty, &elem_ty);
          }
          InferTy::Array { ty: Box::new(elem_ty), len: exprs.len(), span: expr.span }
        }
      }

      ExprKind::Index { object, index } => {
        let obj_ty = self.check_expr(object, env, &Expectation::none());
        let idx_ty = self.check_expr(index, env, &Expectation::none());

        // Index must be usize until we support operator overloading
        self.unify(&idx_ty, &InferTy::Primitive(PrimitiveKind::USize, index.span));

        match self.infcx.resolve(&obj_ty) {
          InferTy::Array { ty, .. } => *ty,
          InferTy::Slice(ty, span) => *ty,
          _ => {
            // TODO: Report error - not indexable
            InferTy::Err(expr.span)
          }
        }
      }

      ExprKind::Field { object, field } => {
        let obj_ty = self.check_expr(object, env, &Expectation::none());
        self.check_field_access(&obj_ty, field, object.span)
      }

      ExprKind::Assign { target, value, .. } => {
        let target_ty = self.check_expr(target, env, &Expectation::none());
        let value_ty =
          self.check_expr(value, env, &Expectation::has_type(target_ty.clone()));
        self.unify(&value_ty, &target_ty);
        InferTy::Unit(expr.span)
      }

      ExprKind::Return { value } => {
        if let Some(val) = value {
          self.check_expr(val, env, expect);
        }
        InferTy::Never(expr.span)
      }

      ExprKind::Break { value, .. } => {
        if let Some(val) = value {
          self.check_expr(val, env, &Expectation::none());
        }
        InferTy::Never(expr.span)
      }

      ExprKind::Continue => InferTy::Never(expr.span),

      ExprKind::Struct { def_id, fields } => {
        self.check_struct_init(def_id, fields, env, expr)
      }

      ExprKind::Comptime { args, .. } => {
        let builtin = self
          .resolver
          .get_recorded_comptime_builtin(self.hir.hir_to_node(&expr.hir_id).unwrap());

        if let Some(builtin) = builtin {
          let func = get_builtin(&builtin);

          if func.params.len() != args.len() {
            self.dcx().emit(ArityMismatch {
              span: expr.span,
              callee: format!("builtin function `{}!`", builtin.name()),
              expected: func.params.len(),
              found: args.len(),
            });

            return InferTy::Err(expr.span);
          }

          let mut final_args = vec![];
          for (param, arg) in func.params.iter().zip(args) {
            match param {
              BuiltInParam::Type => {
                if let ComptimeArg::Expr(expr) = arg {
                  self.dcx().emit(FuncArgMismatch {
                    span: expr.span,
                    expected: "a comptime type".to_string(),
                    found: "an expression".to_string(),
                  });
                } else if let ComptimeArg::Type(ty) = arg {
                  final_args.push(FinalComptimeArg::Ty(self.lower_hir_ty(ty)));
                }
              }
              BuiltInParam::Expr(ty) => {
                if let ComptimeArg::Type(ty) = arg {
                  final_args.push(FinalComptimeArg::Ty(self.lower_hir_ty(ty)));
                } else if let ComptimeArg::Expr(expr) = arg {
                  let expr_ty =
                    self.check_expr(expr, env, &Expectation::ExpectHasType(ty.clone()));
                  let result = self.unify(&expr_ty, ty);

                  match &result {
                    UnifyResult::Err(err) => {
                      if let UnifyError::Mismatch { .. } = err {
                        self.dcx().emit(FuncArgMismatch {
                          span: expr.span,
                          expected: self.format_type(ty),
                          found: self.format_type(&expr_ty),
                        });
                      } else {
                        self.handle_unify_result(result);
                      }
                    }
                    UnifyResult::Ok => {
                      final_args.push(FinalComptimeArg::Expr(*expr.clone()));
                    }
                  }
                }
              }
            }
          }

          self.table.comptime_args.insert(expr.hir_id, final_args);
        }

        InferTy::Err(expr.span)
      }

      _ => {
        warn!("not handled in type checker {expr:#?}");
        // TODO: Handle remaining expression kinds
        self.infcx.fresh(expr.span)
      }
    };

    self.table.record_node_type(expr.hir_id, ty.clone());
    ty
  }

  fn check_literal_with_span(&self, lit: &Literal, span: Span) -> InferTy {
    match lit {
      Literal::Int { suffix, .. } => {
        if let Some(suffix) = suffix {
          InferTy::Primitive(suffix.to_primitive_kind(), span)
        } else {
          self.infcx.fresh_int(span)
        }
      }
      Literal::Float { suffix, .. } => {
        if let Some(suffix) = suffix {
          InferTy::Primitive(suffix.to_primitive_kind(), span)
        } else {
          self.infcx.fresh_float(span)
        }
      }
      Literal::Bool(_) => InferTy::Primitive(PrimitiveKind::Bool, span),
      Literal::Char(_) => InferTy::Primitive(PrimitiveKind::Char, span),
      Literal::String(_) => InferTy::Ptr {
        mutability: Mutability::Const,
        ty: Box::new(InferTy::Primitive(PrimitiveKind::U8, span)),
        span,
      },
      Literal::Unit => InferTy::Unit(span),
    }
  }

  pub fn check_path(
    &self,
    id: DefId,
    env: &TypeEnv,
    explicit_args: Option<&[InferTy]>,
  ) -> InferTy {
    if let Some(cnst) = self.hir.get_const(id) {
      let _ = self
        .new_interpreter(InterpreterMode::Const, InterpreterContext::Const)
        .evaluate_const(&cnst);
    }

    if let Some(ty) = env.lookup(id) {
      return ty.clone();
    }

    if let Some(scheme) = self.table.def_type(id) {
      if let Some(args) = explicit_args {
        if !args.is_empty() && args.len() == scheme.vars.len() {
          return self.instantiate_with_args(&scheme, args).0;
        }
      }
      return self.instantiate(&scheme).0;
    }

    InferTy::Err(Default::default())
  }

  fn instantiate(&self, scheme: &TypeScheme) -> (InferTy, SubstitutionMap) {
    if scheme.vars.is_empty() {
      return (scheme.ty.clone(), SubstitutionMap::new());
    }

    let mut subst: SubstitutionMap = SubstitutionMap::new();
    for &var in &scheme.vars {
      subst.add(var, self.infcx.fresh(scheme.ty.span()));
    }

    (self.apply_subst(&scheme.ty, &subst), subst)
  }

  fn instantiate_with_args(
    &self,
    scheme: &TypeScheme,
    args: &[InferTy],
  ) -> (InferTy, SubstitutionMap) {
    if scheme.vars.is_empty() {
      return (scheme.ty.clone(), SubstitutionMap::new());
    }

    let mut subst: SubstitutionMap = SubstitutionMap::new();
    for (var, arg) in scheme.vars.iter().zip(args.iter()) {
      subst.add(*var, arg.clone());
    }

    (self.apply_subst(&scheme.ty, &subst), subst)
  }

  fn apply_subst(&self, ty: &InferTy, subst: &SubstitutionMap) -> InferTy {
    match ty {
      InferTy::Var(var, span) => ty.clone(),
      InferTy::Param(p) => {
        if let Some(replacement) = subst.get(p.def_id) {
          replacement.clone()
        } else {
          ty.clone()
        }
      }
      InferTy::Adt { def_id, args, span } => InferTy::Adt {
        def_id: *def_id,
        args: args.iter().map(|t| self.apply_subst(t, subst)).collect(),
        span: *span,
      },
      InferTy::Ptr { mutability, ty: inner, span } => InferTy::Ptr {
        mutability: *mutability,
        ty: Box::new(self.apply_subst(inner, subst)),
        span: *span,
      },
      InferTy::Optional(inner, span) => {
        InferTy::Optional(Box::new(self.apply_subst(inner, subst)), *span)
      }
      InferTy::Array { ty: inner, len, span } => InferTy::Array {
        ty: Box::new(self.apply_subst(inner, subst)),
        len: *len,
        span: *span,
      },
      InferTy::Slice(inner, span) => {
        InferTy::Slice(Box::new(self.apply_subst(inner, subst)), *span)
      }
      InferTy::Tuple(tys, span) => {
        InferTy::Tuple(tys.iter().map(|t| self.apply_subst(t, subst)).collect(), *span)
      }
      InferTy::Fn { params, ret, span } => InferTy::Fn {
        params: params.iter().map(|t| self.apply_subst(t, subst)).collect(),
        ret: Box::new(self.apply_subst(ret, subst)),
        span: *span,
      },
      _ => ty.clone(),
    }
  }

  fn check_struct_init(
    &mut self,
    def_id: &DefId,
    fields: &Vec<FieldInit>,
    env: &mut TypeEnv,
    expr: &Expr,
  ) -> InferTy {
    let def = self.resolver.get_definition(*def_id).unwrap();

    if !matches!(def.kind, DefKind::Struct | DefKind::Variant) {
      self.dcx().emit(InvalidStructInit { span: expr.span, found: def.kind.to_string() });
      return InferTy::Err(expr.span);
    }

    let scheme = self.table.def_type(*def_id).unwrap();
    let (def_ty, subst) = self.instantiate(&scheme);

    if DefKind::Struct == def.kind {
      let strukt = self.hir.get_struct(def.id).unwrap().clone();

      for field in fields {
        if !strukt.has_field(field.name) {
          self.dcx().emit(NoFieldOnType {
            span: field.span,
            field: field.name,
            ty: self.format_type(&def_ty),
          });
        } else {
          let field_ty = self.apply_subst(
            &self.table.field_type(*def_id, &field.name.text()).unwrap(),
            &subst,
          );
          let arg_ty =
            self.check_expr(&field.value, env, &Expectation::has_type(field_ty.clone()));

          let result = self.unify(&arg_ty, &field_ty);
          if let UnifyResult::Err(err) = &result {
            match err {
              UnifyError::Mismatch { .. } => self.dcx().emit(FieldInitMismatch {
                expected: self.format_type(&field_ty),
                found: self.format_type(&arg_ty),
                span: field.span,
              }),
              _ => self.handle_unify_result(result),
            }
          }
        }
      }

      if !scheme.vars.is_empty() {
        self.table.record_monomorphization(*def_id, subst);
      }
    }

    def_ty
  }

  fn check_call(
    &mut self,
    callee: &Expr,
    args: &[Expr],
    env: &mut TypeEnv,
    span: Span,
    _call_hir_id: HirId,
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
        if args.len() != params.len() {
          // TODO: Report arity mismatch error
        }
        for (arg, param_ty) in args.iter().zip(params.iter()) {
          let arg_ty =
            self.check_expr(arg, env, &Expectation::has_type(param_ty.clone()));
          let result = self.unify(param_ty, &arg_ty);

          warn!("{:#?}", result)
        }

        if let Some(def_id) = callee_def_id {
          if let Some(scheme) = self.table.def_type(def_id) {
            if !scheme.vars.is_empty() {
              let mut subst = SubstitutionMap::new();
              if !explicit_type_args.is_empty()
                && explicit_type_args.len() == scheme.vars.len()
              {
                for (var, arg) in scheme.vars.iter().zip(explicit_type_args.iter()) {
                  subst.add(*var, self.infcx.resolve(arg));
                }
              } else {
                if let InferTy::Fn { params: inst_params, ret: inst_ret, .. } = &callee_ty
                {
                  if let InferTy::Fn { params: scheme_params, ret: scheme_ret, .. } =
                    &scheme.ty
                  {
                    for (scheme_param, inst_param) in
                      scheme_params.iter().zip(inst_params.iter())
                    {
                      self.collect_param_substitutions(
                        scheme_param,
                        &self.infcx.resolve(inst_param),
                        &scheme.vars,
                        &mut subst,
                      );
                    }
                    self.collect_param_substitutions(
                      scheme_ret,
                      &self.infcx.resolve(inst_ret),
                      &scheme.vars,
                      &mut subst,
                    );
                  }
                }
              }
              self.table.record_monomorphization(def_id, subst);
            }
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
        self.unify(&callee_ty, &expected_fn);
        ret_ty
      }
      _ => {
        // TODO: Report error - not callable
        InferTy::Err(span)
      }
    }
  }

  fn collect_param_substitutions(
    &self,
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
      (InferTy::Ptr { ty: s_ty, .. }, InferTy::Ptr { ty: r_ty, .. }) => {
        self.collect_param_substitutions(s_ty, r_ty, vars, subst);
      }
      (InferTy::Optional(s_ty, _), InferTy::Optional(r_ty, _)) => {
        self.collect_param_substitutions(s_ty, r_ty, vars, subst);
      }
      (InferTy::Array { ty: s_ty, .. }, InferTy::Array { ty: r_ty, .. }) => {
        self.collect_param_substitutions(s_ty, r_ty, vars, subst);
      }
      (InferTy::Slice(s_ty, _), InferTy::Slice(r_ty, _)) => {
        self.collect_param_substitutions(s_ty, r_ty, vars, subst);
      }
      (InferTy::Tuple(s_tys, _), InferTy::Tuple(r_tys, _)) => {
        for (s, r) in s_tys.iter().zip(r_tys.iter()) {
          self.collect_param_substitutions(s, r, vars, subst);
        }
      }
      (
        InferTy::Fn { params: s_params, ret: s_ret, .. },
        InferTy::Fn { params: r_params, ret: r_ret, .. },
      ) => {
        for (s, r) in s_params.iter().zip(r_params.iter()) {
          self.collect_param_substitutions(s, r, vars, subst);
        }
        self.collect_param_substitutions(s_ret, r_ret, vars, subst);
      }
      (InferTy::Adt { args: s_args, .. }, InferTy::Adt { args: r_args, .. }) => {
        for (s, r) in s_args.iter().zip(r_args.iter()) {
          self.collect_param_substitutions(s, r, vars, subst);
        }
      }
      _ => {}
    }
  }

  fn check_field_access(
    &self,
    obj_ty: &InferTy,
    field: &Identifier,
    _span: Span,
  ) -> InferTy {
    let resolved = self.infcx.resolve(obj_ty);

    match resolved {
      InferTy::Adt { def_id, span, .. } => {
        if let Some(field_ty) = self.table.field_type(def_id, &field.text()) {
          field_ty
        } else {
          // TODO: Report unknown field error
          InferTy::Err(span)
        }
      }
      InferTy::Tuple(tys, span) => {
        // Check for tuple field access (e.g., tuple.0)
        if let Ok(idx) = field.text().parse::<usize>() {
          if idx < tys.len() {
            return tys[idx].clone();
          }
        }
        // TODO: Report error
        InferTy::Err(span)
      }
      _ => {
        // TODO: Report error - no fields on this type
        InferTy::Err(_span)
      }
    }
  }

  fn check_pattern(&mut self, pat: &Pat, expected: &InferTy, env: &mut TypeEnv) {
    match &pat.kind {
      PatKind::Binding { def_id, .. } => {
        env.bind(*def_id, expected.clone());
        self.table.record_node_type(pat.hir_id, expected.clone());
      }
      PatKind::Wild => {
        self.table.record_node_type(pat.hir_id, expected.clone());
      }
      PatKind::Tuple(pats) => {
        if let InferTy::Tuple(tys, _) = expected {
          for (p, ty) in pats.iter().zip(tys.iter()) {
            self.check_pattern(p, ty, env);
          }
        } else {
          // TODO: Report pattern mismatch
        }
      }
      PatKind::Literal(_) => {
        // TODO: Check literal pattern
        self.table.record_node_type(pat.hir_id, expected.clone());
      }
      _ => {
        // TODO: Handle other patterns
      }
    }
  }

  pub fn debug_print_resolved_types(&self) {
    eprintln!("\n=== Substitution Map ===");
    for entry in self.infcx.substitution.iter() {
      let var = entry.key();
      let ty = entry.value();
      eprintln!("  {:?} -> {:?}", var, self.format_type(ty));
    }

    eprintln!("\n=== Type Variable Kinds ===");
    for entry in self.infcx.var_kinds.iter() {
      let var = entry.key();
      let kind = entry.value();
      eprintln!("  {:?} => {:?}", var, kind);
    }

    eprintln!("\n=== Resolved Node Types ===");
    for entry in self.table.node_types.iter() {
      let hir_id = entry.key();
      let ty = entry.value();
      let resolved = self.infcx.resolve(ty);
      eprintln!("  {:?} => {:?}", hir_id, self.format_type(&resolved));
    }
    eprintln!("\n=== Type Schemes ===");
    for entry in self.table.def_types.iter() {
      let def_id = entry.key();
      let scheme = entry.value();
      eprintln!("  {:?} => {:?}", def_id, scheme);
    }
    eprintln!();

    eprintln!("\n=== Monomorphizations ===");
    for mono in self.table.monomorphizations.iter() {
      let def_id = mono.key();
      let scheme = mono.value();
      eprintln!("  {:?} => {:?}", def_id, scheme);
    }
    eprintln!();
  }
}
