use crate::queries;
use crate::BCtx;
use boron_types::ast::module::Modules;
use boron_types::ast::NodeId;
use boron_types::resolver::def::{DefId, Definition};
use boron_types::resolver::import_order::ImportGraph;
use boron_types::resolver::resolver::Resolver;

queries! {
    #[no_cache]
    query modules(): &'ctx Modules;

    #[no_cache]
    query import_graph(): &'ctx ImportGraph;

    #[no_cache]
    query resolver(): &'ctx Resolver;

    #[no_cache]
    query get_definition(id: DefId): Option<Definition>;

    #[no_cache]
    query get_resolution(id: NodeId): Option<DefId>;
}
