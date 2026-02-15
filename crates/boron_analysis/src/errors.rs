use crate::TyVar;
use boron_diagnostic_macro::Diagnostic;
use boron_session::prelude::Span;
use boron_source::ident_table::Identifier;

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
#[error("cannot apply unary operator `{op}` to `{ty}`")]
#[code(CONST_EVAL_INVALID_UNARY_OP)]
pub struct InvalidUnaryOp {
  #[error("in this expr")]
  pub span: Span,
  pub op: String,
  pub ty: String,
}

#[derive(Diagnostic)]
#[error("cannot apply binary operator `{op}` to `{lhs}` and `{rhs}`")]
#[code(CONST_EVAL_INVALID_BINARY_OP)]
pub struct InvalidBinaryOp {
  #[error("in this expr")]
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
pub struct NoFieldForStructInit {
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
#[error("function body expected `{expected}`, but found `{found}`")]
#[code(TYPE_CHECKER_RETURN_TYPE_MISMATCH)]
pub struct ReturnTypeMismatch {
  #[error("expected return type")]
  pub expected_span: Span,
  pub expected: String,
  #[error("but body evaluates to `{found}`")]
  pub body_span: Option<Span>,
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

#[derive(Diagnostic)]
#[error("type `{ty}` cannot be dereferenced")]
#[code(TYPE_CHECKER_TY_CANT_BE_DEREFERENCED)]
pub struct TyCantBeDereferenced {
  #[error("error while trying to dereference this")]
  pub span: Span,
  pub ty: String,
}

#[derive(Diagnostic)]
#[error(
  "logical not (`!`) cannot be applied to numeric values. use bitwise not (`~`) for numerics"
)]
#[code(TYPE_CHECKER_NOT_ON_NUMERIC)]
pub struct UnaryNotOnNumeric {
  // TODO: add suggestion when implemented
  #[error("in this expression")]
  pub span: Span,
}

#[derive(Diagnostic)]
#[error("struct pattern doesn't cover all fields: {fields}")]
#[code(TYPE_CHECKER_STRUCT_PAT_NOT_ALL_FIELDS)]
#[help("use `..` to cover all remaining fields")]
pub struct NotAllFieldsCovered {
  pub fields: String,
  #[error("in this pattern")]
  pub span: Span,
}

#[derive(Diagnostic)]
#[error("refutable pattern in local binding")]
#[code(TYPE_CHECKER_LOCAL_PATTERN_IS_REFUTABLE)]
pub struct RefutablePatternInLocalBinding {
  #[error("in this pattern")]
  pub span: Span,
}

#[derive(Diagnostic)]
#[error("no value passed for parameter `{param}`")]
#[code(TYPE_CHECKER_NO_VALUE_PASSED_FOR_PARAMETER)]
pub struct NoValuePassedForParameter {
  #[error("in this function call")]
  pub func_call: Span,
  #[help_label("for this parameter")]
  pub param_span: Span,
  pub param: Identifier,
}

#[derive(Diagnostic)]
#[error("no field named `{field}` on type `{ty}`")]
#[code(TYPE_CHECKER_NO_FILED_ON_TY)]
pub struct NoFieldForTy {
  #[error("in this field")]
  pub span: Span,
  pub field: Identifier,
  pub ty: String,
}

#[derive(Diagnostic)]
#[error("cannot call {callee}")]
#[code(TYPE_CHECKER_CANNOT_CALL)]
pub struct CannotCall {
  #[error("here")]
  pub span: Span,
  pub callee: String,
}

#[derive(Diagnostic)]
#[error("no method `{method}` found on type `{ty}`")]
#[code(TYPE_CHECKER_NO_METHOD_FOR_TY)]
pub struct NoMethodForTy {
  #[error("in this method call")]
  pub span: Span,
  pub method: Identifier,
  pub ty: String,
}
