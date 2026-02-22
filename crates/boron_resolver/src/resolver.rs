use crate::builtin_kind::BuiltInKind;
use crate::def::{DefId, Definition};
use crate::import_order::ImportGraph;
use crate::ribs::Rib;
use crate::scope::{ScopeId, ScopeKind, Scopes};
use crate::symbol::SymbolTable;
use boron_parser::module::Modules;
use boron_parser::{NodeId, Path};
use boron_source::ident_table::Identifier;
use boron_source::prelude::SourceFileId;
use dashmap::DashMap;
use dashmap::mapref::one::Ref;
use parking_lot::RwLock;

/// The main resolver structure.
/// This struct contains all shared state for name resolution.
#[derive(Debug)]
pub struct Resolver {
  /// All scopes in the program
  pub scopes: Scopes,
  /// The symbol table mapping names to definitions
  pub symbols: SymbolTable,
  /// All definitions in the program
  pub definitions: DashMap<DefId, Definition>,
  /// The import dependency graph
  pub import_graph: ImportGraph,
  /// Path to `SourceFileId` mapping
  pub path_to_files: DashMap<NodeId, SourceFileId>,
  /// Per-file exported symbols for cross-module resolution
  pub module_exports_values: DashMap<SourceFileId, DashMap<Identifier, DefId>>,
  /// Per-file exported symbols for cross-module resolution
  pub module_exports_types: DashMap<SourceFileId, DashMap<Identifier, DefId>>,
  /// Per-DefId exported symbols for inline modules
  pub inline_module_exports_values: DashMap<DefId, DashMap<Identifier, DefId>>,
  /// Per-DefId exported symbols for inline modules
  pub inline_module_exports_types: DashMap<DefId, DashMap<Identifier, DefId>>,
  pub adt_members: DashMap<DefId, DashMap<Identifier, DefId>>,
  module_ribs: DashMap<SourceFileId, RwLock<(ScopeId, Rib)>>,
  pub comptime_using_builtins: DashMap<NodeId, BuiltInKind>,
  pub self_to_struct: DashMap<DefId, DefId>,
}

impl Resolver {
  pub fn new() -> Self {
    Self {
      scopes: Scopes::new(),
      symbols: SymbolTable::new(),
      definitions: DashMap::new(),
      import_graph: ImportGraph::new(),
      module_exports_values: DashMap::new(),
      module_exports_types: DashMap::new(),
      inline_module_exports_values: DashMap::new(),
      inline_module_exports_types: DashMap::new(),
      adt_members: DashMap::new(),
      path_to_files: DashMap::new(),
      module_ribs: DashMap::new(),
      comptime_using_builtins: DashMap::new(),
      self_to_struct: DashMap::new(),
    }
  }

  pub fn build_import_graph(&self, modules: &Modules) {
    for module_ref in modules.all() {
      let source_file = module_ref.source_file_id;
      self.import_graph.add_module(source_file);

      self.module_exports_values.entry(source_file).or_insert_with(DashMap::new);
      self.module_exports_types.entry(source_file).or_insert_with(DashMap::new);
    }
  }

  pub fn record_comptime_builtin(&self, node: NodeId, kind: BuiltInKind) {
    self.comptime_using_builtins.insert(node, kind);
  }

  pub fn get_recorded_comptime_builtin(&self, node: NodeId) -> Option<BuiltInKind> {
    self.comptime_using_builtins.get(&node).map(|b| b.clone())
  }

  pub fn add_import_edge(&self, from: SourceFileId, to: SourceFileId) {
    self.import_graph.add_import(from, to);
  }

  pub fn add_definition(&self, def: Definition) -> DefId {
    let id = def.id;
    self.definitions.insert(id, def);
    id
  }

  pub fn get_definition(&self, id: DefId) -> Option<Definition> {
    self.definitions.get(&id).map(|r| r.clone())
  }

  pub fn create_scope(
    &self,
    parent: Option<ScopeId>,
    kind: ScopeKind,
    source_file: SourceFileId,
    owner: Option<DefId>,
  ) -> ScopeId {
    self.scopes.create(parent, kind, source_file, owner)
  }

  pub fn create_module_scope(&self, source_file: SourceFileId) -> ScopeId {
    self.scopes.create_module_scope(source_file)
  }

  pub fn parent_scope(&self, id: ScopeId) -> Option<ScopeId> {
    self.scopes.parent(id)
  }

  pub fn save_module_rib(&self, file: SourceFileId, scope_id: ScopeId, rib: Rib) {
    self.module_ribs.insert(file, RwLock::new((scope_id, rib)));
  }

  pub fn get_module_rib(&self, file: SourceFileId) -> Option<(ScopeId, Rib)> {
    self.module_ribs.get(&file).map(|entry| entry.read().clone())
  }

  pub fn export_value(&self, module: SourceFileId, name: Identifier, def_id: DefId) {
    self
      .module_exports_values
      .entry(module)
      .or_insert_with(DashMap::new)
      .insert(name, def_id);
  }

  pub fn export_type(&self, module: SourceFileId, name: Identifier, def_id: DefId) {
    self
      .module_exports_types
      .entry(module)
      .or_insert_with(DashMap::new)
      .insert(name, def_id);
  }

  pub fn lookup_module_value(
    &self,
    module: SourceFileId,
    name: &Identifier,
  ) -> Option<DefId> {
    let exports = self.module_exports_values.get(&module)?;
    exports.get(name).map(|r| *r)
  }

  pub fn lookup_module_type(
    &self,
    module: SourceFileId,
    name: &Identifier,
  ) -> Option<DefId> {
    let exports = self.module_exports_types.get(&module)?;
    exports.get(name).map(|r| *r)
  }

  /// Export a value from an inline module
  pub fn export_inline_value(&self, module_def: DefId, name: Identifier, def_id: DefId) {
    self
      .inline_module_exports_values
      .entry(module_def)
      .or_insert_with(DashMap::new)
      .insert(name, def_id);
  }

  /// Export a type from an inline module
  pub fn export_inline_type(&self, module_def: DefId, name: Identifier, def_id: DefId) {
    self
      .inline_module_exports_types
      .entry(module_def)
      .or_insert_with(DashMap::new)
      .insert(name, def_id);
  }

  /// Look up a value in an inline module's exports
  pub fn lookup_inline_module_value(
    &self,
    module_def: DefId,
    name: &Identifier,
  ) -> Option<DefId> {
    let exports = self.inline_module_exports_values.get(&module_def)?;
    exports.get(name).map(|r| *r)
  }

  /// Look up a type in an inline module's exports
  pub fn lookup_inline_module_type(
    &self,
    module_def: DefId,
    name: &Identifier,
  ) -> Option<DefId> {
    let exports = self.inline_module_exports_types.get(&module_def)?;
    exports.get(name).map(|r| *r)
  }

  pub fn add_adt_member(&self, struct_def: DefId, name: Identifier, def_id: DefId) {
    self.adt_members.entry(struct_def).or_insert_with(DashMap::new).insert(name, def_id);
  }

  pub fn lookup_adt_member(&self, struct_def: DefId, name: &Identifier) -> Option<DefId> {
    let members = self.adt_members.get(&struct_def)?;
    members.get(name).map(|r| *r)
  }

  pub fn lookup_adt_parent(&self, child: DefId) -> Option<DefId> {
    self
      .adt_members
      .iter()
      .find(|parent| parent.value().iter().any(|c| c.value() == &child))
      .map(|p| *p.key())
  }

  pub fn find_parent(&self, child_id: DefId) -> Option<DefId> {
    self
      .adt_members
      .iter()
      .find(|members| members.iter().any(|member| *member == child_id))
      .map(|def| *def.key())
  }

  pub fn lookup_file_for_path(&self, path: &Path) -> Option<SourceFileId> {
    self.path_to_files.get(&path.id).map(|f| *f)
  }

  pub fn add_path_mapping(&self, path: &Path, source_file_id: SourceFileId) {
    self.path_to_files.insert(path.id, source_file_id);
  }

  pub fn add_self_mapping(&self, self_id: DefId, struct_id: DefId) {
    self.self_to_struct.insert(self_id, struct_id);
  }

  pub fn get_self_mapping(&'_ self, self_id: DefId) -> Option<Ref<'_, DefId, DefId>> {
    self.self_to_struct.get(&self_id)
  }
}

impl Default for Resolver {
  fn default() -> Self {
    Self::new()
  }
}
