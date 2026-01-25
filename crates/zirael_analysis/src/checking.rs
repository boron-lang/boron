use crate::errors::{
  FieldInitMismatch, InvalidStructInit, NoFieldOnType, VarInitMismatch,
};
use crate::interpreter::{
  Interpreter, InterpreterCache, InterpreterContext, InterpreterMode,
};
use crate::table::{InferCtx, TypeEnv, TypeTable};
use crate::ty::{Expectation, InferTy, TyVar, TypeScheme};
use crate::{UnifyError, UnifyResult};
use std::collections::HashMap;
use zirael_diagnostics::DiagnosticCtx;
use zirael_hir::expr::{FieldInit, PathExpr};
use zirael_hir::{
  Block, Const, Expr, ExprKind, Function, Hir, Literal, ParamKind, Pat, PatKind, Stmt,
  StmtKind,
};
use zirael_parser::Mutability;
use zirael_parser::ast::types::PrimitiveKind;
use zirael_resolver::{DefId, DefKind, Resolver};
use zirael_utils::context::Context;
use zirael_utils::ident_table::Identifier;
use zirael_utils::prelude::{Span, warn};

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
      match &param.kind {
        ParamKind::Regular { ty, .. } => {
          let param_ty = self.lower_hir_ty(ty);
          env.bind(param.def_id, param_ty);
        }
        ParamKind::Variadic { ty, .. } => {
          let param_ty = self.lower_hir_ty(ty);
          env.bind(param.def_id, InferTy::Slice(Box::new(param_ty), param.span));
        }
        ParamKind::SelfParam { .. } => {
          // TODO: Bind self parameter
        }
      }
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

      ExprKind::Path(path) => self.check_path(path, env),

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

      ExprKind::Call { callee, args } => self.check_call(callee, args, env, expr.span),

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

      _ => {
        warn!("not handled in type checker {expr:#?}");
        // TODO: Handle remaining expression kinds
        self.infcx.fresh(expr.span)
      }
    };

    self.table.record_node_type(expr.hir_id, ty.clone());
    ty
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

    let def_ty =
      self.table.def_types.get(def_id).expect("no def type found").value().clone().ty;

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
          let field_ty = self.table.field_type(*def_id, &field.name.text()).unwrap();
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
    }

    def_ty
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

  fn check_path(&self, path: &PathExpr, env: &TypeEnv) -> InferTy {
    if let Some(cnst) = self.hir.get_const(path.def_id) {
      let _ = self
        .new_interpreter(InterpreterMode::Const, InterpreterContext::Const)
        .evaluate_const(&cnst);
    }

    if let Some(ty) = env.lookup(path.def_id) {
      return ty.clone();
    }

    if let Some(scheme) = self.table.def_type(path.def_id) {
      return self.instantiate(&scheme);
    }

    InferTy::Err(Default::default())
  }

  fn instantiate(&self, scheme: &TypeScheme) -> InferTy {
    if scheme.vars.is_empty() {
      return scheme.ty.clone();
    }

    let mut subst: HashMap<TyVar, InferTy> = HashMap::new();
    for &var in &scheme.vars {
      subst.insert(var, self.infcx.fresh(scheme.ty.span()));
    }

    self.apply_subst(&scheme.ty, &subst)
  }

  fn apply_subst(&self, ty: &InferTy, subst: &HashMap<TyVar, InferTy>) -> InferTy {
    match ty {
      InferTy::Var(var, span) => {
        if let Some(replacement) = subst.get(var) {
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

  fn check_call(
    &mut self,
    callee: &Expr,
    args: &[Expr],
    env: &mut TypeEnv,
    span: Span,
  ) -> InferTy {
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
          self.unify(param_ty, &arg_ty);
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
}
