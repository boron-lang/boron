mod builtin;
mod checking;
mod errors;
pub mod interpreter;
mod literals;
mod table;
mod ty;
mod unify;
pub mod validator;

pub use checking::{TyChecker, typeck_hir};
pub use table::{Constraint, ConstraintKind, InferCtx, TypeEnv, TypeTable};
pub use ty::{
  Expectation, InferTy, TyVar, TyVarKind, TypeScheme, UnifyError, UnifyResult,
};
