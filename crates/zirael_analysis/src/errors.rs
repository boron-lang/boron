use crate::{InferTy, TyVar};
use zirael_diagnostic_macro::Diagnostic;
use zirael_utils::prelude::Span;

#[derive(Diagnostic)]
#[error("array length needs to be a number found `{found}`")]
#[error(TYPE_CHECKER_ARRAY_LENGTH_NOT_A_NUMBER)]
pub struct ArrayLenNotANumber {
  #[error("in this expression")]
  pub span: Span,
  pub found: String,
}

#[derive(Diagnostic)]
#[error("invalid unary operation")]
#[code(CONST_EVAL_INVALID_UNARY_OP)]
pub struct InvalidUnaryOp {
  #[error("cannot apply unary operator `{op}` to `{ty}`")]
  pub span: Span,
  pub op: String,
  pub ty: String,
}

#[derive(Diagnostic)]
#[error("invalid binary operation")]
#[code(CONST_EVAL_INVALID_BINARY_OP)]
pub struct InvalidBinaryOp {
  #[error("cannot apply binary operator `{op}` to `{lhs}` and `{rhs}`")]
  pub span: Span,
  pub op: String,
  pub lhs: String,
  pub rhs: String,
}

#[derive(Diagnostic)]
#[error("attempted to divide `{value}` by 0")]
#[code(CONST_EVAL_DIVISION_BY_ZERO)]
pub struct DivisionByZero {
  #[error("attempt to divide by zero")]
  pub span: Span,
  pub value: i128,
}

#[derive(Diagnostic)]
#[error("shift amount must be in range 0..{max_shift} for `{ty}`")]
#[code(CONST_EVAL_SHIFT_OUT_OF_RANGE)]
pub struct ShiftOutOfRange {
  #[error("shift amount is {amount}")]
  pub span: Span,
  pub ty: String,
  pub amount: i128,
  pub max_shift: i32,
}

#[derive(Diagnostic)]
#[error("unsupported expression in constant evaluation")]
#[code(CONST_EVAL_UNSUPPORTED_EXPR)]
pub struct UnsupportedConstExpr {
  #[error("this expression cannot be evaluated at compile time")]
  pub span: Span,
}

#[derive(Diagnostic)]
#[error("expected the path to lead to a const")]
#[code(CONST_EVAL_PATH_ISNT_CONST)]
pub struct PathNotConst {
  #[error("in here")]
  pub span: Span,
}

#[derive(Diagnostic)]
#[error(
  "attempted to initialize variable with `{found}`, but expected `{expected}`"
)]
#[code(TYPE_CHECKER_VAR_INIT_MISMATCH)]
pub struct VarInitMismatch {
  #[error("in this variable")]
  pub span: Span,
  pub expected: String,
  pub found: String,
}

#[derive(Diagnostic)]
#[error(
  "occurs check failed: type variable `{var}` occurs in `{ty}`, causing infinite type"
)]
#[code(TYPE_CHECKER_OCCURS_CHECK)]
pub struct OccursCheck {
  #[error("at this type variable")]
  pub var_span: Span,
  pub var: TyVar,
  #[error("within this type")]
  pub ty_span: Span,
  pub ty: String,
}

#[derive(Diagnostic)]
#[error("array length mismatch: expected `{expected}`, found `{found}`")]
#[code(TYPE_CHECKER_ARRAY_LEN_MISMATCH)]
pub struct ArrayLenMismatch {
  #[error("at this array")]
  pub span: Span,
  pub expected: usize,
  pub found: usize,
}
