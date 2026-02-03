use crate::exprs::{Block, Expr, ExprKind, FieldInit, Local, MatchArm, Stmt, StmtKind};
use crate::items::{Field, Function, Struct};
use crate::Param;
use boron_analysis::float::construct_float;
use boron_analysis::int::construct_i128;
use boron_analysis::interpreter::{
  Interpreter, InterpreterCache, InterpreterContext, InterpreterMode,
};
use boron_analysis::literal_table::FullLiteral;
use boron_analysis::{InferTy, TypeTable};
use boron_diagnostics::DiagnosticCtx;
use boron_hir::expr::{
  ComptimeArg as HirComptimeArg, FieldInit as HirFieldInit, PathExpr as HirPathExpr,
};
use boron_hir::{
  Block as HirBlock, Expr as HirExpr, ExprKind as HirExprKind, Function as HirFunction,
  Hir, HirId, Literal, Local as HirLocal, MatchArm as HirMatchArm, Stmt as HirStmt,
  StmtKind as HirStmtKind, Struct as HirStruct,
};
use boron_parser::{AssignOp, BinaryOp, Mutability, UnaryOp};
use boron_resolver::{DefId, DefKind, Resolver};
use boron_source::span::Span;
use boron_utils::ident_table::Identifier;
use dashmap::DashMap;

#[derive(Debug, Default)]
pub struct Thir {
  pub functions: DashMap<DefId, Function>,
  pub structs: DashMap<DefId, Struct>,
}

impl Thir {
  pub fn new() -> Self {
    Self::default()
  }
}

#[derive(Debug)]
pub struct ThirLowerer<'a> {
  pub hir: &'a Hir,
  resolver: &'a Resolver,
  dcx: &'a DiagnosticCtx,
  type_table: &'a TypeTable,
  interpreter_cache: InterpreterCache,
  thir: Thir,
}

impl<'a> ThirLowerer<'a> {
  pub fn new(
    hir: &'a Hir,
    resolver: &'a Resolver,
    dcx: &'a DiagnosticCtx,
    type_table: &'a TypeTable,
  ) -> Self {
    Self {
      hir,
      resolver,
      dcx,
      type_table,
      interpreter_cache: InterpreterCache::new(),
      thir: Thir::new(),
    }
  }

  pub fn lower(mut self) -> Thir {
    for func in &self.hir.functions {
      let lowered = self.lower_function(func.value());
      self.thir.functions.insert(*func.key(), lowered);
    }

    for strukt in &self.hir.structs {
      let lowered = self.lower_struct(strukt.value());
      self.thir.structs.insert(*strukt.key(), lowered);
    }

    self.thir
  }

  pub fn new_interpreter(&'a self) -> Interpreter<'a> {
    Interpreter::new(
      self.dcx,
      &self.interpreter_cache,
      self.resolver,
      self.hir,
      InterpreterMode::Const,
      InterpreterContext::Other,
    )
  }

  pub fn lower_function(&mut self, func: &HirFunction) -> Function {
    let mut params = vec![];

    for param in &func.params {
      let ty =
        self.type_table.node_type(param.hir_id).unwrap_or(InferTy::Err(param.span));

      params.push(Param {
        hir_id: param.hir_id,
        def_id: param.def_id,
        ty,
        span: param.span,
      });
    }

    let body = func.body.as_ref().map(|b| self.lower_block(b));
    let scheme = self.type_table.def_type(func.def_id).expect("should be known");

    let return_type = match &scheme.ty {
      InferTy::Fn { ret, .. } => ret.as_ref().clone(),
      _ => scheme.ty.clone(),
    };

    Function {
      hir_id: func.hir_id,
      def_id: func.def_id,
      name: func.name,
      generics: func.generics.clone(),
      params,
      return_type,
      body,
      span: func.span,
    }
  }

  pub fn lower_struct(&self, strukt: &HirStruct) -> Struct {
    let fields = strukt
      .fields
      .iter()
      .map(|f| {
        let ty = self
          .type_table
          .field_type(strukt.def_id, &f.name.to_string())
          .unwrap_or(InferTy::Err(f.span));

        Field { hir_id: f.hir_id, name: f.name, ty, span: f.span }
      })
      .collect();

    Struct {
      hir_id: strukt.hir_id,
      def_id: strukt.def_id,
      name: strukt.name,
      generics: strukt.generics.clone(),
      fields,
      items: strukt.items.clone(),
      span: strukt.span,
    }
  }

  pub fn lower_block(&mut self, block: &HirBlock) -> Block {
    let stmts = block.stmts.iter().map(|s| self.lower_stmt(s)).collect();
    let expr = block.expr.as_ref().map(|e| Box::new(self.lower_expr(e)));

    Block { hir_id: block.hir_id, stmts, expr, span: block.span }
  }

  pub fn lower_stmt(&mut self, stmt: &HirStmt) -> Stmt {
    let kind = match &stmt.kind {
      HirStmtKind::Local(local) => StmtKind::Local(Box::new(self.lower_local(local))),
      HirStmtKind::Expr(expr) => StmtKind::Expr(self.lower_expr(expr)),
      HirStmtKind::Semi(expr) => StmtKind::Semi(self.lower_expr(expr)),
    };

    Stmt { hir_id: stmt.hir_id, kind, span: stmt.span }
  }

  pub fn lower_local(&mut self, local: &HirLocal) -> Local {
    let ty = self.type_table.node_type(local.hir_id);

    Local {
      hir_id: local.hir_id,
      def_id: local.def_id,
      pat: local.pat.clone(),
      ty,
      init: local.init.as_ref().map(|e| self.lower_expr(e)),
      span: local.span,
    }
  }

  pub fn lower_expr(&mut self, expr: &HirExpr) -> Expr {
    let kind = match &expr.kind {
      HirExprKind::Literal(lit) => self.lower_literal(lit, expr.span),
      HirExprKind::Path(path) => self.lower_path(path, expr),
      HirExprKind::Binary { op, lhs, rhs } => self.lower_binary(*op, lhs, rhs, expr),
      HirExprKind::Unary { op, operand } => self.lower_unary(*op, operand, expr),
      HirExprKind::Assign { op, target, value } => self.lower_assign(*op, target, value),
      HirExprKind::Cast { expr: inner, ty: _ } => self.lower_cast(inner, expr.hir_id),
      HirExprKind::Call { callee, args } => self.lower_call(callee, args),
      HirExprKind::Comptime { callee, args } => self.lower_comptime(callee, args, expr),
      HirExprKind::MethodCall { receiver, method, args } => {
        self.lower_method_call(receiver, method, args)
      }
      HirExprKind::Field { object, field } => self.lower_field(object, field),
      HirExprKind::Index { object, index } => self.lower_index(object, index),
      HirExprKind::AddrOf { mutability, operand } => {
        self.lower_addr_of(*mutability, operand)
      }
      HirExprKind::Struct { def_id, fields } => self.lower_struct_expr(*def_id, fields),
      HirExprKind::Tuple(exprs) => self.lower_tuple(exprs),
      HirExprKind::Array(exprs, len) => self.lower_array(exprs, len.as_deref()),
      HirExprKind::Block(block) => self.lower_block_expr(block),
      HirExprKind::If { condition, then_block, else_branch } => {
        self.lower_if(condition, then_block, else_branch.as_deref())
      }
      HirExprKind::Match { scrutinee, arms } => self.lower_match(scrutinee, arms),
      HirExprKind::Loop { body } => self.lower_loop(body),
      HirExprKind::Break { value } => self.lower_break(value.as_deref()),
      HirExprKind::Continue => self.lower_continue(),
      HirExprKind::Return { value } => self.lower_return(value.as_deref()),
      HirExprKind::Err => ExprKind::Err,
    };

    Expr { hir_id: expr.hir_id, kind, span: expr.span }
  }

  fn lower_literal(&self, lit: &Literal, span: Span) -> ExprKind {
    let full_lit = match lit {
      Literal::Int { base, value, suffix } => {
        FullLiteral::Int(construct_i128(self.dcx, *base, value, span))
      }
      Literal::Float { value, suffix } => {
        FullLiteral::Float(construct_float(self.dcx, value, span))
      }
      Literal::Bool(b) => FullLiteral::Bool(*b),
      Literal::Char(c) => FullLiteral::Char(*c),
      Literal::String(s) => FullLiteral::String(s.clone()),
      Literal::Unit => FullLiteral::Unit,
    };
    ExprKind::Literal(full_lit)
  }

  fn lower_path(&self, path: &HirPathExpr, expr: &HirExpr) -> ExprKind {
    if let Some(cnst) = self.hir.get_const(path.def_id) {
      let interpreter = Interpreter::new(
        self.dcx,
        &self.interpreter_cache,
        self.resolver,
        self.hir,
        InterpreterMode::Const,
        InterpreterContext::Const,
      );
      let value = interpreter.evaluate_expr(&cnst.value);
      if let Some(lit) = Self::const_value_to_literal(value) {
        return ExprKind::Literal(lit);
      }
    }

    if let Some(def) = self.resolver.get_definition(path.def_id) {
      match def.kind {
        DefKind::Local | DefKind::Param => {
          return ExprKind::LocalRef(path.def_id);
        }
        _ => {}
      }
    }

    ExprKind::Path(path.def_id)
  }

  fn lower_binary(
    &mut self,
    op: BinaryOp,
    lhs: &HirExpr,
    rhs: &HirExpr,
    expr: &HirExpr,
  ) -> ExprKind {
    if let Some(lit) = self.try_const_fold(expr) {
      return ExprKind::Literal(lit);
    }

    ExprKind::Binary {
      op,
      lhs: Box::new(self.lower_expr(lhs)),
      rhs: Box::new(self.lower_expr(rhs)),
    }
  }

  fn lower_unary(&mut self, op: UnaryOp, operand: &HirExpr, expr: &HirExpr) -> ExprKind {
    if let Some(lit) = self.try_const_fold(expr) {
      return ExprKind::Literal(lit);
    }

    ExprKind::Unary { op, operand: Box::new(self.lower_expr(operand)) }
  }

  fn lower_assign(
    &mut self,
    op: AssignOp,
    target: &HirExpr,
    value: &HirExpr,
  ) -> ExprKind {
    ExprKind::Assign {
      op,
      target: Box::new(self.lower_expr(target)),
      value: Box::new(self.lower_expr(value)),
    }
  }

  fn lower_cast(&mut self, expr: &HirExpr, hir_id: HirId) -> ExprKind {
    let ty = self.type_table.node_type(hir_id).unwrap_or(InferTy::Err(expr.span));

    ExprKind::Cast { expr: Box::new(self.lower_expr(expr)), ty }
  }

  fn lower_call(&mut self, callee: &HirExpr, args: &[HirExpr]) -> ExprKind {
    let callee_def_id = match &callee.kind {
      HirExprKind::Path(path) => path.def_id,
      _ => todo!("handle correctly"),
    };

    let args = args.iter().map(|a| self.lower_expr(a)).collect();
    ExprKind::Call { callee: callee_def_id, args }
  }

  fn lower_comptime(
    &mut self,
    callee: &HirExpr,
    args: &[HirComptimeArg],
    expr: &HirExpr,
  ) -> ExprKind {
    todo!("evaluate value")
  }

  fn lower_method_call(
    &mut self,
    receiver: &HirExpr,
    method: &Identifier,
    args: &[HirExpr],
  ) -> ExprKind {
    todo!()
  }

  fn lower_field(&mut self, object: &HirExpr, field: &Identifier) -> ExprKind {
    ExprKind::Field { object: Box::new(self.lower_expr(object)), field: *field }
  }

  fn lower_index(&mut self, object: &HirExpr, index: &HirExpr) -> ExprKind {
    ExprKind::Index {
      object: Box::new(self.lower_expr(object)),
      index: Box::new(self.lower_expr(index)),
    }
  }

  fn lower_addr_of(&mut self, _mutability: Mutability, operand: &HirExpr) -> ExprKind {
    ExprKind::AddrOf { operand: Box::new(self.lower_expr(operand)) }
  }

  fn lower_struct_expr(&mut self, def_id: DefId, fields: &[HirFieldInit]) -> ExprKind {
    let fields = fields.iter().map(|f| self.lower_field_init(f)).collect();
    ExprKind::Struct { def_id, fields }
  }

  fn lower_tuple(&mut self, exprs: &[HirExpr]) -> ExprKind {
    ExprKind::Tuple(exprs.iter().map(|e| self.lower_expr(e)).collect())
  }

  fn lower_array(&mut self, exprs: &[HirExpr], repeat: Option<&HirExpr>) -> ExprKind {
    ExprKind::Array(exprs.iter().map(|e| self.lower_expr(e)).collect())
  }

  fn lower_block_expr(&mut self, block: &HirBlock) -> ExprKind {
    ExprKind::Block(self.lower_block(block))
  }

  fn lower_if(
    &mut self,
    condition: &HirExpr,
    then_block: &HirBlock,
    else_branch: Option<&HirExpr>,
  ) -> ExprKind {
    ExprKind::If {
      condition: Box::new(self.lower_expr(condition)),
      then_block: self.lower_block(then_block),
      else_branch: else_branch.map(|e| Box::new(self.lower_expr(e))),
    }
  }

  fn lower_match(&mut self, scrutinee: &HirExpr, arms: &[HirMatchArm]) -> ExprKind {
    ExprKind::Match {
      scrutinee: Box::new(self.lower_expr(scrutinee)),
      arms: arms.iter().map(|a| self.lower_hir_match_arm(a)).collect(),
    }
  }

  fn lower_loop(&mut self, body: &HirBlock) -> ExprKind {
    ExprKind::Loop { body: self.lower_block(body) }
  }

  fn lower_break(&mut self, value: Option<&HirExpr>) -> ExprKind {
    ExprKind::Break { value: value.map(|e| Box::new(self.lower_expr(e))) }
  }

  fn lower_continue(&mut self) -> ExprKind {
    ExprKind::Continue
  }

  fn lower_return(&mut self, value: Option<&HirExpr>) -> ExprKind {
    ExprKind::Return { value: value.map(|e| Box::new(self.lower_expr(e))) }
  }

  fn lower_hir_match_arm(&mut self, arm: &HirMatchArm) -> MatchArm {
    MatchArm {
      hir_id: arm.hir_id,
      pat: arm.pat.clone(),
      guard: arm.guard.as_ref().map(|g| self.lower_expr(g)),
      body: self.lower_expr(&arm.body),
      span: arm.span,
    }
  }

  fn lower_field_init(&mut self, field: &HirFieldInit) -> FieldInit {
    FieldInit {
      hir_id: field.hir_id,
      name: field.name,
      value: self.lower_expr(&field.value),
      span: field.span,
    }
  }
}
