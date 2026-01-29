pub mod builtins;
pub mod expander;
pub mod functions;
pub mod results;

use crate::TypeTable;
use crate::builtin::expander::BuiltInExpander;
use crate::results::BuiltInResults;
use boron_hir::Hir;
use boron_resolver::Resolver;
use boron_utils::context::Context;

pub fn expand_builtins<'a>(
  ctx: &'a Context<'a>,
  resolver: &'a Resolver,
  type_table: &'a TypeTable,
  hir: &'a Hir,
) -> BuiltInResults {
  let expander = BuiltInExpander::new(ctx, resolver, type_table, hir);

  for func in &hir.functions {
    expander.expand_function(&func)
  }

  expander.results
}
