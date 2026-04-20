mod collector;
pub mod errors;
mod import_resolution;
mod module_resolver;
mod visitor;

pub use boron_types::resolver::def::{DefId, DefKind, Definition};
pub use boron_types::resolver::import_order::{ImportGraph, ResolutionOrder};
pub use boron_types::resolver::resolver::Resolver;
pub use boron_types::resolver::ribs::{Rib, RibKind};
pub use boron_types::resolver::scope::{Scope, ScopeId, ScopeKind, Scopes};
pub use boron_types::resolver::symbol::{Symbol, SymbolKind, SymbolTable};
pub use module_resolver::ModuleResolver;
pub use visitor::ResolveVisitor;

pub mod prelude {
  pub use crate::module_resolver::ModuleResolver;
  pub use crate::visitor::ResolveVisitor;
  pub use boron_types::resolver::builtin_kind::*;
  pub use boron_types::resolver::def::{DefId, DefKind, Definition};
  pub use boron_types::resolver::import_order::{ImportGraph, ResolutionOrder};
  pub use boron_types::resolver::resolver::Resolver;
  pub use boron_types::resolver::ribs::{Rib, RibKind};
  pub use boron_types::resolver::scope::{Scope, ScopeId, ScopeKind, Scopes};
  pub use boron_types::resolver::symbol::{Symbol, SymbolKind, SymbolTable};
}
