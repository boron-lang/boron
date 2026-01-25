mod builtin;
mod checking;
mod collector;
mod errors;
pub mod interpreter;
mod literals;
mod table;
mod ty;
mod ty_formatter;
mod ty_lower;
mod unify;
pub mod validator;
mod vars;

pub use checking::{TyChecker, typeck_hir};
pub use table::{Constraint, ConstraintKind, InferCtx, TypeEnv, TypeTable};
pub use ty::{
  Expectation, InferTy, TyVar, TyVarKind, TypeScheme, UnifyError, UnifyResult,
};
