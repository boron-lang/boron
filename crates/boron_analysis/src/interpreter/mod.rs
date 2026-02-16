mod stack;
pub mod utils;
pub mod values;

use crate::errors::{
  DivisionByZero, PathNotConst, ShiftOutOfRange, UnsupportedConstExpr,
};
use crate::float::construct_float;
use crate::interpreter::stack::Stack;
use crate::interpreter::utils::InterpreterLimits;
use crate::interpreter::values::ConstValue;
use crate::literals::int::construct_i128;
use boron_diagnostics::DiagnosticCtx;
use boron_hir::{Const, Expr, ExprKind, Hir, HirId, Literal};
use boron_parser::{BinaryOp, InterpreterMode, UnaryOp};
use boron_resolver::Resolver;
use dashmap::DashMap;
use std::cmp::PartialEq;

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum InterpreterContext {
  ArrayLen,
  ArrayRepeat,
  EnumDiscriminant,
  Other,
  Const,
}

#[derive(Debug)]
pub struct InterpreterCache(DashMap<HirId, ConstValue>);

impl Default for InterpreterCache {
  fn default() -> Self {
    Self::new()
  }
}

impl InterpreterCache {
  pub fn new() -> Self {
    Self(DashMap::new())
  }

  pub fn map(&self) -> &DashMap<HirId, ConstValue> {
    &self.0
  }

  pub fn insert(&self, id: HirId, result: ConstValue) {
    self.map().insert(id, result);
  }

  pub fn get(&self, id: HirId) -> Option<ConstValue> {
    self.map().get(&id).map(|v| v.clone())
  }
}

#[derive(Debug)]
pub struct Interpreter<'a> {
  dcx: &'a DiagnosticCtx,
  cache: &'a InterpreterCache,
  resolver: &'a Resolver,
  hir: &'a Hir,

  mode: InterpreterMode,
  limits: InterpreterLimits,
  context: InterpreterContext,

  stack: Stack,
}

impl<'a> Interpreter<'a> {
  pub fn new(
    dcx: &'a DiagnosticCtx,
    cache: &'a InterpreterCache,
    resolver: &'a Resolver,
    hir: &'a Hir,
    mode: InterpreterMode,
    context: InterpreterContext,
  ) -> Self {
    Self {
      resolver,
      dcx,
      hir,
      stack: Stack::new(),
      limits: Default::default(),
      mode,
      context,
      cache,
    }
  }

  pub fn is_const(&self) -> bool {
    self.mode == InterpreterMode::Const
  }

  pub fn is_runtime(&self) -> bool {
    self.mode == InterpreterMode::Runtime
  }

  pub fn stack(&self) -> &Stack {
    &self.stack
  }

  pub fn evaluate_const(&self, cnst: &Const) -> ConstValue {
    if let Some(expr) = self.cache.get(cnst.hir_id) {
      return expr;
    }

    let value = self.evaluate_expr(&cnst.value);
    self.cache.map().insert(cnst.hir_id, value.clone());

    value
  }

  fn values_equal(lval: &ConstValue, rval: &ConstValue) -> bool {
    match (lval, rval) {
      (ConstValue::Int(l), ConstValue::Int(r)) => l == r,
      (ConstValue::Bool(l), ConstValue::Bool(r)) => l == r,
      (ConstValue::Char(l), ConstValue::Char(r)) => l == r,
      (ConstValue::String(l), ConstValue::String(r)) => l == r,
      (ConstValue::Unit, ConstValue::Unit) => true,
      (ConstValue::Array(l), ConstValue::Array(r)) => {
        l.len() == r.len()
          && l.iter().zip(r.iter()).all(|(a, b)| Self::values_equal(a, b))
      }
      (ConstValue::Struct(l), ConstValue::Struct(r)) => {
        l.len() == r.len()
          && l.iter().zip(r.iter()).all(|(a, b)| Self::values_equal(a, b))
      }
      (
        ConstValue::Enum { tag: t1, payload: p1 },
        ConstValue::Enum { tag: t2, payload: p2 },
      ) => {
        t1 == t2
          && match (p1, p2) {
            (None, None) => true,
            (Some(a), Some(b)) => Self::values_equal(a, b),
            _ => false,
          }
      }
      _ => false,
    }
  }

  fn eval_unary(&self, expr: &Expr, op: &UnaryOp, operand: &Expr) -> ConstValue {
    let val = self.evaluate_expr(operand);

    match op {
      UnaryOp::Plus => val,
      UnaryOp::Neg => {
        if let ConstValue::Int(i) = val {
          ConstValue::Int(-i)
        } else {
          ConstValue::Poison
        }
      }
      UnaryOp::Not => {
        if let ConstValue::Bool(b) = val {
          ConstValue::Bool(!b)
        } else {
          ConstValue::Poison
        }
      }
      UnaryOp::BitNot => {
        if let ConstValue::Int(i) = val {
          ConstValue::Int(!i)
        } else {
          ConstValue::Poison
        }
      }
      UnaryOp::Deref => {
        self.dcx.emit(UnsupportedConstExpr { span: expr.span });
        ConstValue::Poison
      }
      UnaryOp::AddrOf { .. } => unreachable!(),
    }
  }

  fn eval_arithmetic(
    &self,
    expr: &Expr,
    op: &BinaryOp,
    lval: &ConstValue,
    rval: &ConstValue,
  ) -> ConstValue {
    use BinaryOp::{Add, Div, Mod, Mul, Sub};

    match op {
      Add => match (lval, rval) {
        (ConstValue::Int(l), ConstValue::Int(r)) => ConstValue::Int(l.wrapping_add(*r)),
        (ConstValue::String(l), ConstValue::String(r)) => {
          ConstValue::String(format!("{l}{r}"))
        }
        _ => ConstValue::Poison,
      },
      Sub => {
        if let (ConstValue::Int(l), ConstValue::Int(r)) = (lval, rval) {
          ConstValue::Int(l.wrapping_sub(*r))
        } else {
          ConstValue::Poison
        }
      }
      Mul => {
        if let (ConstValue::Int(l), ConstValue::Int(r)) = (lval, rval) {
          ConstValue::Int(l.wrapping_mul(*r))
        } else {
          ConstValue::Poison
        }
      }
      Div => {
        if let (ConstValue::Int(l), ConstValue::Int(r)) = (lval, rval) {
          if *r == 0 {
            self.dcx.emit(DivisionByZero { span: expr.span, value: *l });
            ConstValue::Poison
          } else {
            ConstValue::Int(l.wrapping_div(*r))
          }
        } else {
          ConstValue::Poison
        }
      }
      Mod => {
        if let (ConstValue::Int(l), ConstValue::Int(r)) = (lval, rval) {
          if *r == 0 {
            self.dcx.emit(DivisionByZero { span: expr.span, value: *l });
            ConstValue::Poison
          } else {
            ConstValue::Int(l.wrapping_rem(*r))
          }
        } else {
          ConstValue::Poison
        }
      }
      _ => unreachable!(),
    }
  }

  fn eval_comparison(
    &self,
    expr: &Expr,
    op: &BinaryOp,
    lval: &ConstValue,
    rval: &ConstValue,
  ) -> ConstValue {
    use BinaryOp::{Eq, Ge, Gt, Le, Lt, Ne};

    match op {
      Eq => ConstValue::Bool(Self::values_equal(lval, rval)),
      Ne => ConstValue::Bool(!Self::values_equal(lval, rval)),
      Lt => match (lval, rval) {
        (ConstValue::Int(l), ConstValue::Int(r)) => ConstValue::Bool(l < r),
        (ConstValue::Char(l), ConstValue::Char(r)) => ConstValue::Bool(l < r),
        _ => ConstValue::Poison,
      },
      Le => match (lval, rval) {
        (ConstValue::Int(l), ConstValue::Int(r)) => ConstValue::Bool(l <= r),
        (ConstValue::Char(l), ConstValue::Char(r)) => ConstValue::Bool(l <= r),
        _ => ConstValue::Poison,
      },
      Gt => match (lval, rval) {
        (ConstValue::Int(l), ConstValue::Int(r)) => ConstValue::Bool(l > r),
        (ConstValue::Char(l), ConstValue::Char(r)) => ConstValue::Bool(l > r),
        _ => ConstValue::Poison,
      },
      Ge => match (lval, rval) {
        (ConstValue::Int(l), ConstValue::Int(r)) => ConstValue::Bool(l >= r),
        (ConstValue::Char(l), ConstValue::Char(r)) => ConstValue::Bool(l >= r),
        _ => ConstValue::Poison,
      },
      _ => unreachable!(),
    }
  }

  fn eval_logical(
    &self,
    expr: &Expr,
    op: &BinaryOp,
    lval: &ConstValue,
    rval: &ConstValue,
  ) -> ConstValue {
    use BinaryOp::{And, Or};

    match op {
      And => {
        if let (ConstValue::Bool(l), ConstValue::Bool(r)) = (lval, rval) {
          ConstValue::Bool(*l && *r)
        } else {
          ConstValue::Poison
        }
      }
      Or => {
        if let (ConstValue::Bool(l), ConstValue::Bool(r)) = (lval, rval) {
          ConstValue::Bool(*l || *r)
        } else {
          ConstValue::Poison
        }
      }
      _ => unreachable!(),
    }
  }

  fn eval_bitwise(
    &self,
    expr: &Expr,
    op: &BinaryOp,
    lval: &ConstValue,
    rval: &ConstValue,
  ) -> ConstValue {
    use BinaryOp::{BitAnd, BitOr, BitXor, Shl, Shr};

    match op {
      BitAnd => match (lval, rval) {
        (ConstValue::Int(l), ConstValue::Int(r)) => ConstValue::Int(l & r),
        (ConstValue::Bool(l), ConstValue::Bool(r)) => ConstValue::Bool(l & r),
        _ => ConstValue::Poison,
      },
      BitOr => match (lval, rval) {
        (ConstValue::Int(l), ConstValue::Int(r)) => ConstValue::Int(l | r),
        (ConstValue::Bool(l), ConstValue::Bool(r)) => ConstValue::Bool(l | r),
        _ => ConstValue::Poison,
      },
      BitXor => match (lval, rval) {
        (ConstValue::Int(l), ConstValue::Int(r)) => ConstValue::Int(l ^ r),
        (ConstValue::Bool(l), ConstValue::Bool(r)) => ConstValue::Bool(l ^ r),
        _ => ConstValue::Poison,
      },
      Shl => {
        if let (ConstValue::Int(l), ConstValue::Int(r)) = (lval, rval) {
          if *r < 0 || *r > 127 {
            self.dcx.emit(ShiftOutOfRange {
              span: expr.span,
              amount: *r,
              max_shift: 127,
              ty: "int".to_owned(),
            });
            ConstValue::Poison
          } else {
            ConstValue::Int(l.wrapping_shl(*r as u32))
          }
        } else {
          ConstValue::Poison
        }
      }
      Shr => {
        if let (ConstValue::Int(l), ConstValue::Int(r)) = (lval, rval) {
          if *r < 0 || *r > 127 {
            self.dcx.emit(ShiftOutOfRange {
              span: expr.span,
              amount: *r,
              max_shift: 127,
              ty: "int".to_owned(),
            });
            ConstValue::Poison
          } else {
            ConstValue::Int(l.wrapping_shr(*r as u32))
          }
        } else {
          ConstValue::Poison
        }
      }
      _ => unreachable!(),
    }
  }

  fn eval_binary(
    &self,
    expr: &Expr,
    op: &BinaryOp,
    lhs: &Expr,
    rhs: &Expr,
  ) -> ConstValue {
    let lval = self.evaluate_expr(lhs);
    let rval = self.evaluate_expr(rhs);

    use BinaryOp::{
      Add, And, BitAnd, BitOr, BitXor, Div, Eq, Ge, Gt, Le, Lt, Mod, Mul, Ne, Or, Shl,
      Shr, Sub,
    };
    match op {
      Add | Sub | Mul | Div | Mod => self.eval_arithmetic(expr, op, &lval, &rval),
      Eq | Ne | Lt | Le | Gt | Ge => self.eval_comparison(expr, op, &lval, &rval),
      And | Or => self.eval_logical(expr, op, &lval, &rval),
      BitAnd | BitOr | BitXor | Shl | Shr => self.eval_bitwise(expr, op, &lval, &rval),
    }
  }

  pub fn evaluate_expr(&self, expr: &Expr) -> ConstValue {
    if let Some(expr) = self.cache.get(expr.hir_id) {
      return expr;
    }

    let value = match &expr.kind {
      ExprKind::Literal(lit) => match lit {
        Literal::Int { base, value, .. } => {
          let value = construct_i128(self.dcx, *base, value, expr.span);
          ConstValue::Int(value)
        }
        Literal::Bool(bool) => ConstValue::Bool(*bool),
        Literal::Char(c) => ConstValue::Char(*c),
        Literal::Unit => ConstValue::Unit,
        Literal::String(s) => ConstValue::String(s.clone()),
        Literal::Float { value, .. } => {
          let value = construct_float(self.dcx, value, expr.span);
          ConstValue::Float(value)
        }
      },
      // we don't support & in interpreter so it'll go to the wildcard arm
      ExprKind::Unary { op, operand } if !matches!(op, UnaryOp::AddrOf { .. }) => {
        self.eval_unary(expr, op, operand)
      }
      ExprKind::Binary { op, lhs, rhs } => self.eval_binary(expr, op, lhs, rhs),
      ExprKind::Path(p) => {
        if let Some(cnst) = self.hir.get_const(p.def_id) {
          self.evaluate_expr(&cnst.value)
        } else {
          self.dcx.emit(PathNotConst { span: expr.span });
          ConstValue::Poison
        }
      }
      _ => {
        self.dcx.emit(UnsupportedConstExpr { span: expr.span });
        ConstValue::Poison
      }
    };
    self.cache.map().insert(expr.hir_id, value.clone());

    value
  }
}
