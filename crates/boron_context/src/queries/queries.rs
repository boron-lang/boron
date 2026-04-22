use crate::BCtx;
use boron_queries_macro::queries;
use boron_source::ident_table::Identifier;
use boron_types::ast::module::Modules;
use boron_types::ast::NodeId;
use boron_types::comptime::FinalComptimeArg;
use boron_types::hir::{Const as HirConst, Enum, Function, Generics, Hir, HirId, Struct};
use boron_types::infer_ty::{InferTy, SubstitutionMap, TypeScheme};
use boron_types::resolver::def::{DefId, Definition};
use boron_types::resolver::import_order::ImportGraph;
use boron_types::resolver::resolver::Resolver;
use boron_types::thir::{
    Enum as ThirEnum, Function as ThirFunction, Struct as ThirStruct, Thir,
};
use boron_types::type_table::{MonomorphizationEntry, TypeTable};

queries! {
    fn modules(): &'ctx Modules;
    fn import_graph(): &'ctx ImportGraph;
    fn resolver(): &'ctx Resolver;
    fn get_definition(id: DefId): Option<Definition>;
    fn get_resolution(id: NodeId): Option<DefId>;
    fn hir(): &'ctx Hir;
    fn thir(): &'ctx Thir;
    /// Converts HirId into NodeId and get the node type.
    fn node_type(id: HirId): Option<InferTy>;
    fn comptime_arg(id: HirId): Option<Vec<FinalComptimeArg>>;
    fn def_type(def_id: DefId): Option<TypeScheme>;
    fn record_field_type(def_id: DefId, name: Identifier, ty: InferTy);
    fn record_def_type(def_id: DefId, scheme: TypeScheme);
    fn record_node_type(hir_id: HirId, ty: InferTy): InferTy;
    fn field_type(struct_id: DefId, field_name: Identifier): Option<InferTy>;
    fn adt_parent(id: DefId): Option<DefId>;
    fn hir_const(id: DefId): Option<HirConst>;
    fn self_mapping(id: DefId): Option<DefId>;
    #[cache]
    fn adt_generics(id: DefId): Option<Generics>;
    fn hir_enum(id: DefId): Option<Enum>;
    fn expr_mono(expr_id: HirId): Option<MonomorphizationEntry>;
    fn mono(def_id: DefId): Option<Vec<MonomorphizationEntry>>;
    fn hir_struct(id: DefId): Option<Struct>;
    fn hir_function(id: DefId): Option<Function>;
    fn adt_member(struct_def: DefId, name: &Identifier): Option<DefId>;
    fn record_mono(def_id: DefId, subst: SubstitutionMap);
    fn record_expr_mono(hir_id: HirId, def_id: DefId, subst: SubstitutionMap);
    fn insert_comptime_arg(id: HirId, args: Vec<FinalComptimeArg>);
    /// The type table
    fn tt(): &'ctx TypeTable;
    fn adt_name(def_id: DefId): Option<Identifier>;
    fn thir_enum(def_id: DefId): Option<ThirEnum>;
    fn thir_struct(def_id: DefId): Option<ThirStruct>;
    fn thir_function(def_id: DefId): Option<ThirFunction>;
    fn hir_to_node(hir_id: HirId): Option<NodeId>;
}
