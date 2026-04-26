use crate::queries::queries::Queries;
use crate::BCtx;
use boron_source::ident_table::Identifier;
use boron_source::{PackageId, StableDefId, StablePackageId};
use boron_types::ast::module::Modules;
use boron_types::ast::NodeId;
use boron_types::comptime::FinalComptimeArg;
use boron_types::hir::{
  AdtEntry, Const as HirConst, Enum, Function, Generics, Hir, HirId, Struct,
};
use boron_types::infer_ty::{InferTy, SubstitutionMap, TypeScheme};
use boron_types::ir::Ir;
use boron_types::resolver::def::{DefId, Definition};
use boron_types::resolver::import_order::ImportGraph;
use boron_types::resolver::resolver::Resolver;
use boron_types::thir::{
  Enum as ThirEnum, Function as ThirFunction, Struct as ThirStruct, Thir,
};
use boron_types::type_table::{MonomorphizationEntry, TypeTable};

pub trait QueryProvider<'ctx> {
  fn provide(&self, queries: &mut Queries<'ctx>);
}

impl<'ctx> QueryProvider<'ctx> for BCtx<'ctx> {
  fn provide(&self, queries: &mut Queries<'ctx>) {
    queries.modules = Some(Self::q_modules);
    queries.import_graph = Some(Self::q_import_graph);
    queries.resolver = Some(Self::q_resolver);
    queries.get_definition = Some(Self::q_get_definition);
    queries.get_resolution = Some(Self::q_get_resolution);
    queries.hir = Some(Self::q_hir);
    queries.thir = Some(Self::q_thir);
    queries.node_type = Some(Self::q_node_type);
    queries.comptime_arg = Some(Self::q_comptime_arg);
    queries.def_type = Some(Self::q_def_type);
    queries.record_field_type = Some(Self::q_record_field_type);
    queries.record_def_type = Some(Self::q_record_def_type);
    queries.record_node_type = Some(Self::q_record_node_type);
    queries.field_type = Some(Self::q_field_type);
    queries.adt_parent = Some(Self::q_adt_parent);
    queries.hir_const = Some(Self::q_hir_const);
    queries.self_mapping = Some(Self::q_self_mapping);
    queries.adt_generics = Some(Self::q_adt_generics);
    queries.hir_enum = Some(Self::q_hir_enum);
    queries.expr_mono = Some(Self::q_expr_mono);
    queries.mono = Some(Self::q_mono);
    queries.hir_struct = Some(Self::q_hir_struct);
    queries.hir_function = Some(Self::q_hir_function);
    queries.adt_member = Some(Self::q_adt_member);
    queries.record_mono = Some(Self::q_record_mono);
    queries.record_expr_mono = Some(Self::q_record_expr_mono);
    queries.insert_comptime_arg = Some(Self::q_insert_comptime_arg);
    queries.tt = Some(Self::q_tt);
    queries.adt_name = Some(Self::q_adt_name);
    queries.thir_enum = Some(Self::q_thir_enum);
    queries.thir_struct = Some(Self::q_thir_struct);
    queries.thir_function = Some(Self::q_thir_function);
    queries.hir_to_node = Some(Self::q_hir_to_node);
    queries.current_pkg_id = Some(Self::q_current_pkg_id);
    queries.pkg_id = Some(Self::q_pkg_id);
    queries.stable_def_id = Some(Self::q_stable_def_id);
    queries.is_local = Some(Self::q_is_local);
    queries.set_current_pkg_id = Some(Self::q_set_current_pkg_id);
    queries.ir = Some(Self::q_ir);
  }
}

impl<'ctx> BCtx<'ctx> {
  fn q_is_local(ctx: &'ctx Self, (id,): (DefId,)) -> bool {
    id.package_index == ctx.current_pkg_id()
  }

  fn q_stable_def_id(ctx: &'ctx Self, (id,): (DefId,)) -> StableDefId {
    let stable_pkg_id = ctx
      .id_interner
      .stable_package_id(id.package_index)
      .expect("package id was not interned before stable_def_id query");

    let path = if let Some(def) = ctx.get_definition(id) {
      vec![def.kind.to_string(), def.name.text()]
    } else {
      vec![id.to_string()]
    };

    StableDefId::new(stable_pkg_id, path)
  }

  fn q_pkg_id(ctx: &'ctx Self, (id,): (StablePackageId,)) -> PackageId {
    ctx.id_interner.intern_package_id(id)
  }

  fn q_ir(ctx: &'ctx Self, (): ()) -> &'ctx Ir {
    &ctx.ir
  }

  fn q_set_current_pkg_id(ctx: &'ctx Self, (id,): (PackageId,)) {
    *ctx.current_package.write() = Some(id);
  }

  fn q_current_pkg_id(ctx: &'ctx Self, (): ()) -> PackageId {
    ctx.current_package.read().expect("no package set as current")
  }

  fn q_modules(ctx: &'ctx Self, (): ()) -> &'ctx Modules {
    &ctx.modules
  }

  fn q_import_graph(ctx: &'ctx Self, (): ()) -> &'ctx ImportGraph {
    &ctx.import_graph
  }

  fn q_resolver(ctx: &'ctx Self, (): ()) -> &'ctx Resolver {
    &ctx.resolver
  }

  fn q_get_definition(ctx: &'ctx Self, (id,): (DefId,)) -> Option<Definition> {
    ctx.resolver.definitions.get(&id).map(|def| def.clone())
  }

  fn q_get_resolution(ctx: &'ctx Self, (id,): (NodeId,)) -> Option<DefId> {
    ctx.resolver.symbols.resolutions.get(&id).map(|def_id| *def_id)
  }

  fn q_hir(ctx: &'ctx Self, (): ()) -> &'ctx Hir {
    &ctx.hir
  }

  fn q_thir(ctx: &'ctx Self, (): ()) -> &'ctx Thir {
    &ctx.thir
  }

  fn q_node_type(ctx: &'ctx Self, (id,): (HirId,)) -> Option<InferTy> {
    ctx.table.node_types.get(&id).map(|ty| ty.clone())
  }

  fn q_comptime_arg(ctx: &'ctx Self, (id,): (HirId,)) -> Option<Vec<FinalComptimeArg>> {
    ctx.table.comptime_args.get(&id).map(|args| args.clone())
  }

  fn q_def_type(ctx: &'ctx Self, (def_id,): (DefId,)) -> Option<TypeScheme> {
    ctx.table.def_types.get(&def_id).map(|scheme| scheme.clone())
  }

  fn q_record_field_type(
    ctx: &'ctx Self,
    (def_id, name, ty): (DefId, Identifier, InferTy),
  ) {
    ctx.table.field_types.insert((def_id, name), ty);
  }

  fn q_record_def_type(ctx: &'ctx Self, (def_id, scheme): (DefId, TypeScheme)) {
    ctx.table.def_types.insert(def_id, scheme);
  }

  fn q_record_node_type(ctx: &'ctx Self, (hir_id, ty): (HirId, InferTy)) -> InferTy {
    let ret = ty.clone();
    ctx.table.node_types.insert(hir_id, ty);
    ret
  }

  fn q_field_type(
    ctx: &'ctx Self,
    (struct_id, field_name): (DefId, Identifier),
  ) -> Option<InferTy> {
    ctx.table.field_types.get(&(struct_id, field_name)).map(|ty| ty.clone())
  }

  fn q_adt_parent(ctx: &'ctx Self, (id,): (DefId,)) -> Option<DefId> {
    ctx
      .hir
      .adts
      .iter()
      .find_map(|entry| match entry.value() {
        AdtEntry::Struct(s) if s.items.contains(&id) => Some(*entry.key()),
        AdtEntry::Enum(e)
          if e.items.contains(&id) || e.variants.iter().any(|v| v.def_id == id) =>
        {
          Some(*entry.key())
        }
        _ => None,
      })
      .or_else(|| {
        ctx.resolver.adt_members.iter().find_map(|members| {
          members.value().iter().any(|member| *member == id).then_some(*members.key())
        })
      })
  }

  fn q_hir_const(ctx: &'ctx Self, (id,): (DefId,)) -> Option<HirConst> {
    ctx.hir.consts.get(&id).map(|c| c.clone())
  }

  fn q_self_mapping(ctx: &'ctx Self, (id,): (DefId,)) -> Option<DefId> {
    ctx.resolver.self_to_struct.get(&id).map(|mapping| *mapping)
  }

  fn q_adt_generics(ctx: &'ctx Self, (id,): (DefId,)) -> Option<Generics> {
    ctx.hir.adts.get(&id).map(|adt| match adt.value() {
      AdtEntry::Struct(s) => s.generics.clone(),
      AdtEntry::Enum(e) => e.generics.clone(),
    })
  }

  fn q_hir_enum(ctx: &'ctx Self, (id,): (DefId,)) -> Option<Enum> {
    ctx.hir.adts.get(&id).map(|adt| match adt.value() {
      AdtEntry::Enum(e) => e.clone(),
      AdtEntry::Struct(_) => panic!("expected Enum for {id:?}, found Struct"),
    })
  }

  fn q_expr_mono(ctx: &'ctx Self, (expr_id,): (HirId,)) -> Option<MonomorphizationEntry> {
    ctx.table.expr_monomorphizations.get(&expr_id).map(|m| m.clone())
  }

  fn q_mono(ctx: &'ctx Self, (def_id,): (DefId,)) -> Option<Vec<MonomorphizationEntry>> {
    ctx.table.monomorphizations.get(&def_id).map(|m| m.clone())
  }

  fn q_hir_struct(ctx: &'ctx Self, (id,): (DefId,)) -> Option<Struct> {
    ctx.hir.adts.get(&id).map(|adt| match adt.value() {
      AdtEntry::Struct(s) => s.clone(),
      AdtEntry::Enum(_) => panic!("expected Struct for {id:?}, found Enum"),
    })
  }

  fn q_hir_function(ctx: &'ctx Self, (id,): (DefId,)) -> Option<Function> {
    ctx.hir.functions.get(&id).map(|f| f.clone())
  }

  fn q_adt_member(
    ctx: &'ctx Self,
    (struct_def, name): (DefId, &Identifier),
  ) -> Option<DefId> {
    let members = ctx.resolver.adt_members.get(&struct_def)?;
    members.get(name).map(|def_id| *def_id)
  }

  fn q_record_mono(ctx: &'ctx Self, (def_id, subst): (DefId, SubstitutionMap)) {
    let entry = MonomorphizationEntry { def_id, type_args: subst };
    if let Some(mut existing) = ctx.table.monomorphizations.get_mut(&def_id) {
      existing.push(entry);
    } else {
      ctx.table.monomorphizations.insert(def_id, vec![entry]);
    }
  }

  fn q_record_expr_mono(
    ctx: &'ctx Self,
    (hir_id, def_id, subst): (HirId, DefId, SubstitutionMap),
  ) {
    ctx
      .table
      .expr_monomorphizations
      .insert(hir_id, MonomorphizationEntry { def_id, type_args: subst });
  }

  fn q_insert_comptime_arg(ctx: &'ctx Self, (id, args): (HirId, Vec<FinalComptimeArg>)) {
    ctx.table.comptime_args.insert(id, args);
  }

  fn q_tt(ctx: &'ctx Self, (): ()) -> &'ctx TypeTable {
    &ctx.table
  }

  fn q_adt_name(ctx: &'ctx Self, (def_id,): (DefId,)) -> Option<Identifier> {
    ctx.hir.adts.get(&def_id).map(|adt| match adt.value() {
      AdtEntry::Struct(s) => s.name,
      AdtEntry::Enum(e) => e.name,
    })
  }

  fn q_thir_enum(ctx: &'ctx Self, (def_id,): (DefId,)) -> Option<ThirEnum> {
    ctx.thir.enums.get(&def_id).map(|e| e.clone())
  }

  fn q_thir_struct(ctx: &'ctx Self, (def_id,): (DefId,)) -> Option<ThirStruct> {
    ctx.thir.structs.get(&def_id).map(|s| s.clone())
  }

  fn q_thir_function(ctx: &'ctx Self, (def_id,): (DefId,)) -> Option<ThirFunction> {
    ctx.thir.functions.get(&def_id).map(|f| f.clone())
  }

  fn q_hir_to_node(ctx: &'ctx Self, (hir_id,): (HirId,)) -> Option<NodeId> {
    ctx
      .hir
      .node_to_hir
      .iter()
      .find(|entry| *entry.value() == hir_id)
      .map(|entry| *entry.key())
  }
}
