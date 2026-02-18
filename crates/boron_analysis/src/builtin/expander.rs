use crate::interpreter::values::ConstValue;
use crate::results::BuiltInResults;
use crate::{InferTy, TypeTable};
use boron_diagnostics::DiagnosticCtx;
use boron_hir::expr::{ElseBranch, IfExpr};
use boron_hir::{
  Block, ComptimeCallee, Expr, ExprKind, Function, Hir, ParamKind, StmtKind,
};
use boron_resolver::prelude::BuiltInKind;
use boron_resolver::Resolver;
use boron_session::prelude::{debug, Session};

pub struct BuiltInExpander<'a> {
  pub sess: &'a Session,
  pub resolver: &'a Resolver,
  pub results: BuiltInResults,
  pub hir: &'a Hir,
  pub type_table: &'a TypeTable,
}

impl<'a> BuiltInExpander<'a> {
  pub fn new(
    sess: &'a Session,
    resolver: &'a Resolver,
    type_table: &'a TypeTable,
    hir: &'a Hir,
  ) -> Self {
    Self { sess, results: BuiltInResults::new(), resolver, hir, type_table }
  }

  pub fn dcx(&self) -> &'a DiagnosticCtx {
    self.sess.dcx()
  }

  pub fn expand_function(&self, func: &Function) {
    for param in &func.params {
      if let ParamKind::Regular { default, .. } = &param.kind
        && let Some(default) = default
      {
        self.walk_expr(default);
      }
    }

    if let Some(body) = &func.body {
      self.walk_block(body);
    }
  }

  fn walk_expr(&self, expr: &Expr) {
    match &expr.kind {
      ExprKind::Binary { lhs, rhs, .. } => {
        self.walk_expr(lhs);
        self.walk_expr(rhs);
      }

      ExprKind::Unary { operand, .. } => {
        self.walk_expr(operand);
      }

      ExprKind::Assign { target, value, .. } => {
        self.walk_expr(target);
        self.walk_expr(value);
      }

      ExprKind::Cast { expr, .. } => {
        self.walk_expr(expr);
      }

      ExprKind::Call { callee, args }
      | ExprKind::MethodCall { receiver: callee, args, .. } => {
        self.walk_expr(callee);
        for arg in args {
          self.walk_expr(&arg.value);
        }
      }

      ExprKind::Comptime { callee, args } => {
        if let Some(ty) = self.type_table.node_type(expr.hir_id)
          && let InferTy::Err(_) = ty
        {
          self.results.insert(expr.hir_id, ConstValue::Poison);
          return;
        }

        if let ComptimeCallee::BuiltIn(builtin) = callee {
          let Some(args) = self.type_table.comptime_args.get(&expr.hir_id) else {
            debug!("skipping builtin because there might have been an error before");
            return;
          };

          let result = match builtin {
            BuiltInKind::SizeOf => self.size_of(args[0].as_ty()),
            BuiltInKind::Os => self.os_builtin(),
            _ => todo!("{:#?}", builtin),
          };

          self.results.insert(expr.hir_id, result);
        } else {
          todo!("support comptime")
        }
      }

      ExprKind::Field { object, .. } => {
        self.walk_expr(object);
      }

      ExprKind::Index { object, index } => {
        self.walk_expr(object);
        self.walk_expr(index);
      }

      ExprKind::Struct { fields, .. } => {
        for field in fields {
          self.walk_expr(&field.value);
        }
      }

      ExprKind::Tuple(exprs) | ExprKind::Array(exprs, _) => {
        for e in exprs {
          self.walk_expr(e);
        }
      }

      ExprKind::Block(block) => self.walk_block(block),

      ExprKind::If(if_expr) => self.walk_if(if_expr),

      ExprKind::Match { scrutinee, arms } => {
        self.walk_expr(scrutinee);
        for arm in arms {
          self.walk_expr(&arm.body);
        }
      }

      ExprKind::Loop { body } => {
        self.walk_block(body);
      }

      ExprKind::Break { value } | ExprKind::Return { value } => {
        if let Some(v) = value {
          self.walk_expr(v);
        }
      }

      ExprKind::Literal(_) | ExprKind::Path(_) | ExprKind::Continue | ExprKind::Err => {}
    }
  }

  fn walk_if(&self, if_expr: &IfExpr) {
    self.walk_expr(&if_expr.condition);
    self.walk_block(&if_expr.then_block);
    if let Some(e) = &if_expr.else_branch {
      match e {
        ElseBranch::Block(block) => self.walk_block(block),
        ElseBranch::If(if_expr) => self.walk_if(if_expr),
      }
    }
  }

  fn walk_block(&self, block: &Block) {
    for stmt in &block.stmts {
      match &stmt.kind {
        StmtKind::Local(l) => {
          if let Some(init) = &l.init {
            self.walk_expr(init);
          }
        }
        StmtKind::Expr(expr) => self.walk_expr(expr),
      }
    }

    if let Some(tail) = &block.expr {
      self.walk_expr(tail);
    }
  }
}
