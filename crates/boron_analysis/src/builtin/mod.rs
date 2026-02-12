pub mod builtins;
pub mod expander;
pub mod functions;
pub mod results;

use crate::builtin::expander::BuiltInExpander;
use crate::results::BuiltInResults;
use crate::TypeTable;
use boron_hir::Hir;
use boron_resolver::Resolver;
use boron_session::prelude::Session;

pub fn expand_builtins<'a>(
  sess: &'a Session,
  resolver: &'a Resolver,
  type_table: &'a TypeTable,
  hir: &'a Hir,
) -> BuiltInResults {
  let expander = BuiltInExpander::new(sess, resolver, type_table, hir);

  for func in &hir.functions {
    expander.expand_function(&func);
  }

  expander.results
}
