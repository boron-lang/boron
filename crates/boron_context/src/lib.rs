mod queries;
mod id_interner;

use parking_lot::RwLock;
use boron_source::{PackageId, StablePackageId};
use crate::queries::provider::QueryProvider as _;
use crate::queries::queries::Queries;
use boron_types::ast::module::Modules;
use boron_types::hir::Hir;
use boron_types::ir::Ir;
use boron_types::resolver::import_order::ImportGraph;
use boron_types::resolver::resolver::Resolver;
use boron_types::thir::Thir;
use boron_types::type_table::TypeTable;
use crate::id_interner::IdInterner;

#[derive(Debug, Default)]
pub struct BCtx<'ctx> {
  queries: Queries<'ctx>,
  import_graph: ImportGraph,
  resolver: Resolver,
  modules: Modules,
  hir: Hir,
  thir: Thir,
  ir: Ir,
  table: TypeTable,
  current_package: RwLock<Option<PackageId>>,
  id_interner: IdInterner
}

impl BCtx<'_> {
  pub fn new() -> Self {
    let mut queries = Queries::default();
    Self::default().provide(&mut queries);

    Self { queries, ..Default::default() }
  }
}
