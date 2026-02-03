use crate::TyVar;
use boron_diagnostic_macro::Diagnostic;
use boron_utils::ident_table::Identifier;
use boron_utils::prelude::Span;

#[derive(Diagnostic)]
#[error("array length needs to be a number found `{found}`")]
#[error(TYPE_CHECKER_ARRAY_LENGTH_NOT_A_NUMBER)]
pub struct ArrayLenNotANumber {
  #[error("in this expression")]
  pub span: Span,
  pub found: String,
}

#[derive(Diagnostic)]
#[error("array repeat needs to be a number found `{found}`")]
#[error(TYPE_CHECKER_ARRAY_REPEAT_NOT_A_NUMBER)]
pub struct ArrayRepeatNotANumber {
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
#[error("attempted to initialize variable with `{found}`, but expected `{expected}`")]
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

#[derive(Diagnostic)]
#[error("expected struct or enum variant in struct initialization, found `{found}`")]
#[code(TYPE_CHECKER_INVALID_STRUCT_INIT)]
pub struct InvalidStructInit {
  #[error("in this struct init")]
  pub span: Span,
  pub found: String,
}

#[derive(Diagnostic)]
#[error("no field to initialize named `{field}` on type `{ty}`")]
#[code(TYPE_CHECKER_NO_FIELD)]
pub struct NoFieldOnType {
  #[error("in this field init")]
  pub span: Span,
  pub field: Identifier,
  pub ty: String,
}

#[derive(Diagnostic)]
#[error("attempted to initialize field with `{found}`, but expected `{expected}`")]
#[code(TYPE_CHECKER_FIELD_INIT_MISMATCH)]
pub struct FieldInitMismatch {
  #[error("in this field")]
  pub span: Span,
  pub expected: String,
  pub found: String,
}

#[derive(Diagnostic)]
#[error("{callee} expected `{expected}` arguments, found `{found}`")]
#[code(TYPE_CHECKER_ARITY_MISMATCH)]
pub struct ArityMismatch {
  #[error("in this expr")]
  pub span: Span,
  pub callee: String,
  pub expected: usize,
  pub found: usize,
}

#[derive(Diagnostic)]
#[error("expected {expected}, but found {found} in function argument")]
#[code(TYPE_CHECKER_FUNC_ARG_MISMATCH)]
pub struct FuncArgMismatch {
  #[error("in this argument")]
  pub span: Span,
  pub expected: String,
  pub found: String,
}

#[derive(Diagnostic)]
#[error("cannot infer type for this expression")]
#[code(TYPE_CHECKER_CANNOT_INFER_TYPE)]
pub struct CannotInferType {
  #[error("type must be known at this point")]
  pub span: Span,
}

#[derive(Diagnostic)]
#[error("mismatched types: expected `{expected}`, found `{found}`")]
#[code(TYPE_CHECKER_TYPE_MISMATCH)]
pub struct TypeMismatch {
  #[error("expected `{expected}`")]
  pub expected_span: Span,
  pub expected: String,
  #[error("found `{found}`")]
  pub found_span: Span,
  pub found: String,
}

#[derive(Diagnostic)]
#[error("expected an integer type, found `{found}`")]
#[code(TYPE_CHECKER_NOT_AN_INTEGER)]
pub struct NotAnInteger {
  #[error("expected integer type")]
  pub span: Span,
  pub found: String,
}

#[derive(Diagnostic)]
#[error("expected a float type, found `{found}`")]
#[code(TYPE_CHECKER_NOT_A_FLOAT)]
pub struct NotAFloat {
  #[error("expected float type")]
  pub span: Span,
  pub found: String,
}

#[derive(Diagnostic)]
#[error("mutability mismatch: expected `{expected}`, found `{found}`")]
#[code(TYPE_CHECKER_MUTABILITY_MISMATCH)]
pub struct MutabilityMismatch {
  #[error("mismatched mutability")]
  pub span: Span,
  pub expected: String,
  pub found: String,
}

#[derive(Diagnostic)]
#[error("incompatible type variable kinds: `{kind1}` vs `{kind2}`")]
#[code(TYPE_CHECKER_INCOMPATIBLE_KINDS)]
pub struct IncompatibleKinds {
  #[error("cannot unify these type variables")]
  pub span: Span,
  pub kind1: String,
  pub kind2: String,
}

#[derive(Diagnostic)]
#[error("tuple length mismatch: expected `{expected}` elements, found `{found}`")]
#[code(TYPE_CHECKER_TUPLE_ARITY_MISMATCH)]
pub struct TupleArityMismatch {
  #[error("in this tuple")]
  pub span: Span,
  pub expected: usize,
  pub found: usize,
}

#[derive(Diagnostic)]
#[error("index must be of type `usize`, found `{found}`")]
#[code(TYPE_CHECKER_INDEX_TYPE_MISMATCH)]
pub struct IndexTypeMismatch {
  #[error("in this index expression")]
  pub span: Span,
  pub found: String,
}

#[derive(Diagnostic)]
#[error("cannot assign `{found}` to variable of type `{expected}`")]
#[code(TYPE_CHECKER_ASSIGN_TYPE_MISMATCH)]
pub struct AssignTypeMismatch {
  #[error("expected `{expected}`")]
  pub target_span: Span,
  pub expected: String,
  #[error("found `{found}`")]
  pub value_span: Span,
  pub found: String,
}

#[derive(Diagnostic)]
#[error("function body returns `{found}`, but expected `{expected}`")]
#[code(TYPE_CHECKER_RETURN_TYPE_MISMATCH)]
pub struct ReturnTypeMismatch {
  #[error("expected return type")]
  pub expected_span: Span,
  pub expected: String,
  #[error("but body evaluates to `{found}`")]
  pub body_span: Span,
  pub found: String,
}

#[derive(Diagnostic)]
#[error("constant initialized with `{found}`, but expected `{expected}`")]
#[code(TYPE_CHECKER_CONST_INIT_MISMATCH)]
pub struct ConstInitMismatch {
  #[error("expected type")]
  pub expected_span: Span,
  pub expected: String,
  #[error("but found `{found}`")]
  pub value_span: Span,
  pub found: String,
}
