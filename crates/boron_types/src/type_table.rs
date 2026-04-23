use crate::comptime::FinalComptimeArg;
use crate::hir::HirId;
use crate::infer_ty::{InferTy, SubstitutionMap, TypeScheme};
use boron_source::ident_table::Identifier;
use dashmap::DashMap;
use boron_source::DefId;

#[derive(Debug, Clone)]
pub struct MonomorphizationEntry {
  pub def_id: DefId,
  pub type_args: SubstitutionMap,
}

#[derive(Debug)]
pub struct TypeTable {
  /// Maps HIR nodes to their inferred types
  pub node_types: DashMap<HirId, InferTy>,
  pub def_types: DashMap<DefId, TypeScheme>,
  pub field_types: DashMap<(DefId, Identifier), InferTy>,
  /// Maps method signatures to thier type scheme.
  pub method_types: DashMap<(DefId, String), TypeScheme>,
  /// Maps comptime function call to it's arguments
  pub comptime_args: DashMap<HirId, Vec<FinalComptimeArg>>,
  pub monomorphizations: DashMap<DefId, Vec<MonomorphizationEntry>>,
  pub expr_monomorphizations: DashMap<HirId, MonomorphizationEntry>,
}

impl TypeTable {
  pub fn new() -> Self {
    Self {
      node_types: DashMap::new(),
      def_types: DashMap::new(),
      field_types: DashMap::new(),
      method_types: DashMap::new(),
      comptime_args: DashMap::new(),
      monomorphizations: DashMap::new(),
      expr_monomorphizations: DashMap::new(),
    }
  }

  pub fn record_monomorphization(&self, def_id: DefId, type_args: SubstitutionMap) {
    let entry = MonomorphizationEntry { def_id, type_args };
    if let Some(mut r) = self.monomorphizations.get_mut(&def_id) {
      r.push(entry);
    } else {
      self.monomorphizations.insert(def_id, vec![entry]);
    }
  }

  pub fn record_expr_monomorphization(
    &self,
    expr_id: HirId,
    def_id: DefId,
    type_args: SubstitutionMap,
  ) {
    self
      .expr_monomorphizations
      .insert(expr_id, MonomorphizationEntry { def_id, type_args });
  }

  pub fn expr_monomorphization(&self, expr_id: HirId) -> Option<MonomorphizationEntry> {
    self.expr_monomorphizations.get(&expr_id).map(|m| m.clone())
  }

  pub fn record_node_type(&self, hir_id: HirId, ty: InferTy) {
    self.node_types.insert(hir_id, ty);
  }

  pub fn node_type(&self, hir_id: HirId) -> Option<InferTy> {
    self.node_types.get(&hir_id).map(|t| t.clone())
  }

  pub fn record_def_type(&self, def_id: DefId, scheme: TypeScheme) {
    self.def_types.insert(def_id, scheme);
  }

  pub fn def_type(&self, def_id: DefId) -> Option<TypeScheme> {
    self.def_types.get(&def_id).map(|s| s.clone())
  }

  pub fn record_field_type(&self, struct_id: DefId, field_name: Identifier, ty: InferTy) {
    self.field_types.insert((struct_id, field_name), ty);
  }

  pub fn field_type(&self, struct_id: DefId, field_name: Identifier) -> Option<InferTy> {
    self.field_types.get(&(struct_id, field_name)).map(|t| t.clone())
  }

  pub fn record_method_type(
    &self,
    struct_id: DefId,
    method_name: String,
    scheme: TypeScheme,
  ) {
    self.method_types.insert((struct_id, method_name), scheme);
  }

  pub fn method_type(&self, struct_id: DefId, method_name: &str) -> Option<TypeScheme> {
    self.method_types.get(&(struct_id, method_name.to_owned())).map(|s| s.clone())
  }
}

impl Default for TypeTable {
  fn default() -> Self {
    Self::new()
  }
}
