mod queries;

use crate::queries::queries::Queries;
use boron_types::ast::module::Modules;
use boron_types::ast::NodeId;
use boron_types::hir::Hir;
use boron_types::ir::Ir;
use boron_types::resolver::def::{DefId, Definition};
use boron_types::resolver::import_order::ImportGraph;
use boron_types::resolver::resolver::Resolver;
use boron_types::thir::Thir;

#[derive(Debug, Default)]
pub struct BCtx<'ctx> {
  queries: Queries<'ctx>,
  import_graph: ImportGraph,
  resolver: Resolver,
  modules: Modules,
  hir: Hir,
  thir: Thir,
  ir: Ir,
}

impl<'ctx> BCtx<'ctx> {
  pub fn new() -> Self {
    let mut queries = Queries::default();
    queries.modules = Some(Self::q_modules);
    queries.import_graph = Some(Self::q_import_graph);
    queries.resolver = Some(Self::q_resolver);
    queries.get_definition = Some(Self::q_get_definition);
    queries.get_resolution = Some(Self::q_get_resolution);

    Self { queries, ..Default::default() }
  }

  fn q_modules(ctx: &'ctx BCtx<'ctx>, (): ()) -> &'ctx Modules {
    &ctx.modules
  }

  fn q_import_graph(ctx: &'ctx BCtx<'ctx>, (): ()) -> &'ctx ImportGraph {
    &ctx.import_graph
  }

  fn q_resolver(ctx: &'ctx BCtx<'ctx>, (): ()) -> &'ctx Resolver {
    &ctx.resolver
  }

  fn q_get_definition(ctx: &'ctx BCtx<'ctx>, (id,): (DefId,)) -> Option<Definition> {
    ctx.resolver.get_definition(id)
  }

  fn q_get_resolution(ctx: &'ctx BCtx<'ctx>, (id,): (NodeId,)) -> Option<DefId> {
    ctx.resolver.symbols.get_resolution(id)
  }
}
