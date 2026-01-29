mod builtin;
mod checking;
mod collector;
mod errors;
mod formatter;
pub mod interpreter;
mod literals;
mod size_of;
mod table;
mod ty;
mod ty_lower;
mod unify;
pub mod validator;
mod vars;

pub use builtin::*;
pub use checking::{TyChecker, typeck_hir};
pub use table::{InferCtx, TypeEnv, TypeTable};
pub use ty::{InferTy, TyVar, TyVarKind, TypeScheme};
