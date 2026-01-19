mod errors;
mod types;

use crate::validator::errors::ComptimeNoGenerics;
use zirael_hir::{Block, Expr, ExprKind, Function, Hir, Stmt, StmtKind};
use zirael_resolver::{DefId, Resolver};
use zirael_utils::prelude::DiagnosticCtx;

pub struct ComptimeValidator<'a> {
  pub hir: &'a Hir,
  pub dcx: &'a DiagnosticCtx,
  pub resolver: &'a Resolver,
}

pub fn validate_comptime(hir: &Hir, dcx: &DiagnosticCtx, resolver: &Resolver) {
  let validator = ComptimeValidator { hir, dcx, resolver };

  for entry in hir.functions.iter() {
    let def_id = *entry.key();
    let func = entry.value();
    validator.validate_function(def_id, &func);
  }
}

impl ComptimeValidator<'_> {
  fn validate_function(&self, _def_id: DefId, func: &Function) {
    if func.is_comptime {
      if !func.generics.params.is_empty() {
        self.dcx.emit(ComptimeNoGenerics {
          span: func.generics.span,
        });
      }
    }

    if let Some(body) = &func.body {
      self.validate_block(body);
    }
  }

  fn validate_expr(&self, expr: &Expr) {
    match &expr.kind {
      ExprKind::Binary { op: _, lhs, rhs } => {
        self.validate_expr(lhs);
        self.validate_expr(rhs);
      }

      ExprKind::Unary { op: _, operand } => self.validate_expr(operand),

      ExprKind::Call { callee, args } => {
        self.validate_expr(callee);

        for arg in args {
          self.validate_expr(arg);
        }
      }

      ExprKind::If {
        condition,
        then_block,
        else_branch,
      } => {
        self.validate_expr(condition);

        self.validate_block(then_block);

        if let Some(else_expr) = else_branch {
          self.validate_expr(else_expr);
        }
      }

      ExprKind::Block(block) => self.validate_block(block),

      ExprKind::Array(exprs, _) | ExprKind::Tuple(exprs) => {
        for expr in exprs {
          self.validate_expr(expr);
        }
      }

      ExprKind::Index { object, index } => {
        self.validate_expr(object);
        self.validate_expr(index);
      }

      ExprKind::Field { object, field } => {
        self.validate_expr(object);
      }

      ExprKind::Assign { target, value, .. } => {
        self.validate_expr(target);
        self.validate_expr(value);
      }

      ExprKind::Return { value } => {
        if let Some(val) = value {
          self.validate_expr(val);
        }
      }

      ExprKind::Break { value } => {
        if let Some(val) = value {
          self.validate_expr(val);
        }
      }

      ExprKind::Continue { .. }
      | ExprKind::Path(..)
      | ExprKind::Literal(..) => {}

      ExprKind::Comptime { callee, args } => {
        println!("{:?}", callee);
      }

      _ => {
        todo!("unhandled")
      }
    };
  }

  fn validate_block(&self, block: &Block) {
    for stmt in &block.stmts {
      self.validate_stmt(stmt);
    }

    if let Some(expr) = &block.expr {
      self.validate_expr(expr);
    }
  }

  fn validate_stmt(&self, stmt: &Stmt) {
    match &stmt.kind {
      StmtKind::Local(local) => {
        if let Some(init) = &local.init {
          self.validate_expr(init);
        }
      }
      StmtKind::Expr(expr) => {
        self.validate_expr(expr);
      }
      StmtKind::Semi(expr) => {
        self.validate_expr(expr);
      }
    }
  }
}
