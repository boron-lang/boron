use crate::expr::{
  Block, ComptimeArg, Expr, ExprKind, FieldInit, Literal, Local, MatchArm, PathExpr,
  PathSegment, Stmt, StmtKind,
};
use crate::lower::context::LoweringContext;
use crate::pat::{Pat, PatKind};
use boron_parser::ast::expressions::{
  self, ComptimeArg as AstComptimeArg, ExprKind as AstExprKind,
};
use boron_parser::ast::statements;
use boron_parser::{IntBase, IntSuffix};
use boron_source::prelude::Span;
use boron_utils::prelude::{Identifier, debug};
use expressions::Literal as AstLiteral;
use itertools::Itertools as _;

fn byte_literal(value: u8) -> Literal {
  Literal::Int {
    base: IntBase::Decimal,
    value: value.to_string(),
    suffix: Some(IntSuffix::U8),
  }
}

impl LoweringContext<'_> {
  pub fn lower_expr(&mut self, expr: &expressions::Expr) -> Expr {
    let kind = match &expr.kind {
      AstExprKind::Literal(lit) => {
        if let AstLiteral::ByteString(lit) = lit {
          let values = lit
            .value
            .iter()
            .map(|v| Expr {
              hir_id: self.next_hir_id(),
              kind: ExprKind::Literal(byte_literal(*v)),
              span: expr.span,
            })
            .collect_vec();

          ExprKind::Array(values, None)
        } else if let AstLiteral::Byte(byte) = lit {
          println!("{byte:#?}");
          ExprKind::Literal(byte_literal(byte.value))
        } else {
          ExprKind::Literal(self.lower_literal(lit))
        }
      }

      AstExprKind::Path(path) => {
        let def_id = self.get_def_id(path.id);

        if let Some(def_id) = def_id {
          ExprKind::Path(PathExpr {
            def_id,
            segments: path
              .segments
              .iter()
              .map(|s| PathSegment {
                name: s.identifier,
                args: s.args.iter().map(|a| self.lower_type(a)).collect(),
              })
              .collect(),
          })
        } else {
          debug!("failed to lower a path {path:#?}");
          ExprKind::Err
        }
      }

      AstExprKind::SelfValue => {
        if let Some(def_id) = self.get_def_id(expr.id) {
          ExprKind::Path(PathExpr {
            def_id,
            segments: vec![PathSegment {
              name: Identifier::new("self", Span::dummy()),
              args: vec![],
            }],
          })
        } else {
          debug!("failed to lower self {expr:?}");
          ExprKind::Err
        }
      }

      AstExprKind::Binary { op, left, right } => ExprKind::Binary {
        op: *op,
        lhs: Box::new(self.lower_expr(left)),
        rhs: Box::new(self.lower_expr(right)),
      },

      AstExprKind::Unary { op, operand } => {
        ExprKind::Unary { op: *op, operand: Box::new(self.lower_expr(operand)) }
      }

      AstExprKind::Assign { op, target, value } => ExprKind::Assign {
        op: *op,
        target: Box::new(self.lower_expr(target)),
        value: Box::new(self.lower_expr(value)),
      },

      AstExprKind::Cast { expr, target_type } => ExprKind::Cast {
        expr: Box::new(self.lower_expr(expr)),
        ty: self.lower_type(target_type),
      },

      AstExprKind::Call { callee, args } => ExprKind::Call {
        callee: Box::new(self.lower_expr(callee)),
        args: args.iter().map(|a| self.lower_expr(&a.value)).collect(),
      },

      AstExprKind::Field { object, field } => {
        ExprKind::Field { object: Box::new(self.lower_expr(object)), field: *field }
      }

      AstExprKind::Index { object, index } => ExprKind::Index {
        object: Box::new(self.lower_expr(object)),
        index: Box::new(self.lower_expr(index)),
      },

      AstExprKind::AddrOf { mutability, operand } => ExprKind::AddrOf {
        mutability: *mutability,
        operand: Box::new(self.lower_expr(operand)),
      },

      AstExprKind::Struct { path, fields } => {
        let def_id = self.get_def_id(path.id);

        if let Some(def_id) = def_id {
          ExprKind::Struct {
            def_id,
            fields: fields
              .iter()
              .map(|f| FieldInit {
                hir_id: self.next_hir_id(),
                name: f.name,
                value: self.lower_expr(&f.value),
                span: f.span,
              })
              .collect(),
          }
        } else {
          debug!("failed to lower struct {expr:?}");
          ExprKind::Err
        }
      }

      AstExprKind::Tuple(exprs) => {
        ExprKind::Tuple(exprs.iter().map(|e| self.lower_expr(e)).collect())
      }

      AstExprKind::Array { values, repeat } => {
        let repeat = repeat.as_ref().map(|e| Box::new(self.lower_expr(e)));

        ExprKind::Array(values.iter().map(|e| self.lower_expr(e)).collect(), repeat)
      }

      AstExprKind::Block(block) => ExprKind::Block(self.lower_block(block)),

      AstExprKind::If(if_expr) => self.lower_if_expr(if_expr),

      AstExprKind::Match(match_expr) => ExprKind::Match {
        scrutinee: Box::new(self.lower_expr(&match_expr.scrutinee)),
        arms: match_expr.arms.iter().map(|a| self.lower_match_arm(a)).collect(),
      },

      AstExprKind::Loop(loop_expr) => {
        ExprKind::Loop { body: self.lower_block(&loop_expr.body) }
      }

      AstExprKind::While(_) | AstExprKind::For(_) | AstExprKind::Range(_) => {
        todo!("desugar")
      }

      AstExprKind::Break(break_expr) => ExprKind::Break {
        value: break_expr.value.as_ref().map(|v| Box::new(self.lower_expr(v))),
      },

      AstExprKind::Continue(_) => ExprKind::Continue,

      AstExprKind::Return(ret_expr) => ExprKind::Return {
        value: ret_expr.value.as_ref().map(|v| Box::new(self.lower_expr(v))),
      },

      AstExprKind::Ternary { condition, then_expr, else_expr } => ExprKind::If {
        condition: Box::new(self.lower_expr(condition)),
        then_block: Block {
          hir_id: self.next_hir_id(),
          stmts: vec![],
          expr: Some(Box::new(self.lower_expr(then_expr))),
          span: then_expr.span,
        },
        else_branch: Some(Box::new(self.lower_expr(else_expr))),
      },

      AstExprKind::Comptime { callee, args } => {
        let args = args
          .iter()
          .map(|a| match a {
            AstComptimeArg::Expr(expr) => {
              ComptimeArg::Expr(Box::new(self.lower_expr(expr)))
            }
            AstComptimeArg::Type(ty) => ComptimeArg::Type(self.lower_type(ty)),
          })
          .collect_vec();

        ExprKind::Comptime { callee: Box::new(self.lower_expr(callee)), args }
      }
    };

    let id = self.next_hir_id();
    self.hir.node_to_hir.insert(expr.id, id);
    Expr { hir_id: id, kind, span: expr.span }
  }

  fn lower_if_expr(&mut self, if_expr: &expressions::IfExpr) -> ExprKind {
    let else_branch = match &if_expr.else_branch {
      None => None,
      Some(expressions::ElseBranch::Block(block)) => Some(Box::new(Expr {
        hir_id: self.next_hir_id(),
        kind: ExprKind::Block(self.lower_block(block)),
        span: block.span,
      })),
      Some(expressions::ElseBranch::If(nested_if)) => {
        let kind = self.lower_if_expr(nested_if);
        Some(Box::new(Expr { hir_id: self.next_hir_id(), kind, span: nested_if.span }))
      }
    };

    ExprKind::If {
      condition: Box::new(self.lower_expr(&if_expr.condition)),
      then_block: self.lower_block(&if_expr.then_block),
      else_branch,
    }
  }

  fn lower_match_arm(&mut self, arm: &expressions::MatchArm) -> MatchArm {
    MatchArm {
      hir_id: self.next_hir_id(),
      pat: self.lower_ast_pattern(&arm.pattern),
      guard: None, // TODO: add guard support
      body: self.lower_expr(&arm.body),
      span: arm.span,
    }
  }

  pub fn lower_block(&mut self, block: &statements::Block) -> Block {
    let mut stmts = Vec::new();
    let mut trailing_expr = None;

    for (i, stmt) in block.statements.iter().enumerate() {
      let is_last = i == block.statements.len() - 1;

      match stmt {
        statements::Statement::Expr(expr_stmt) if is_last && !expr_stmt.has_semicolon => {
          trailing_expr = Some(Box::new(self.lower_expr(&expr_stmt.expr)));
        }
        _ => {
          stmts.push(self.lower_stmt(stmt));
        }
      }
    }

    Block { hir_id: self.next_hir_id(), stmts, expr: trailing_expr, span: block.span }
  }

  fn lower_stmt(&mut self, stmt: &statements::Statement) -> Stmt {
    let (kind, span) = match stmt {
      statements::Statement::VarDecl(var) => {
        let def_id = self.get_def_id(var.id).unwrap_or(boron_resolver::DefId(0));

        let local = Local {
          hir_id: self.next_hir_id(),
          def_id,
          pat: Pat {
            hir_id: self.next_hir_id(),
            kind: PatKind::Binding {
              def_id,
              name: var.name,
              is_mut: var.is_mut,
              subpat: None,
            },
            span: *var.name.span(),
          },
          ty: var.ty.as_ref().map(|t| self.lower_type(t)),
          init: Some(self.lower_expr(&var.value)),
          span: var.span,
        };

        (StmtKind::Local(Box::new(local)), var.span)
      }

      statements::Statement::ConstDecl(c) => {
        let def_id = self.get_def_id(c.id).unwrap_or(boron_resolver::DefId(0));

        let local = Local {
          hir_id: self.next_hir_id(),
          def_id,
          pat: Pat {
            hir_id: self.next_hir_id(),
            kind: PatKind::Binding { def_id, name: c.name, is_mut: false, subpat: None },
            span: *c.name.span(),
          },
          ty: c.ty.as_ref().map(|t| self.lower_type(t)),
          init: Some(self.lower_expr(&c.value)),
          span: c.span,
        };

        (StmtKind::Local(Box::new(local)), c.span)
      }

      statements::Statement::Expr(expr_stmt) => {
        let expr = self.lower_expr(&expr_stmt.expr);
        if expr_stmt.has_semicolon {
          (StmtKind::Semi(expr), expr_stmt.span)
        } else {
          (StmtKind::Expr(expr), expr_stmt.span)
        }
      }

      statements::Statement::Block(block) => {
        let hir_block = self.lower_block(block);
        (
          StmtKind::Expr(Expr {
            hir_id: self.next_hir_id(),
            kind: ExprKind::Block(hir_block),
            span: block.span,
          }),
          block.span,
        )
      }
    };

    Stmt { hir_id: self.next_hir_id(), kind, span }
  }

  pub fn lower_literal(&self, lit: &AstLiteral) -> Literal {
    match lit {
      AstLiteral::Int(i) => {
        Literal::Int { value: i.value.clone(), base: i.base, suffix: i.suffix }
      }
      AstLiteral::Float(f) => Literal::Float { value: f.value.clone(), suffix: f.suffix },
      AstLiteral::Bool(b) => Literal::Bool(b.value),
      AstLiteral::Char(c) => Literal::Char(c.value),
      AstLiteral::String(s) => Literal::String(s.value.clone()),
      AstLiteral::Unit(_) => Literal::Unit,
      _ => unreachable!(),
    }
  }

  pub fn get_literal_span(&self, lit: &AstLiteral) -> Span {
    match lit {
      AstLiteral::Int(i) => i.span,
      AstLiteral::Float(f) => f.span,
      AstLiteral::Bool(b) => b.span,
      AstLiteral::Char(c) => c.span,
      AstLiteral::Byte(b) => b.span,
      AstLiteral::ByteString(b) => b.span,
      AstLiteral::String(s) => s.span,
      AstLiteral::Unit(u) => u.span,
    }
  }
}
