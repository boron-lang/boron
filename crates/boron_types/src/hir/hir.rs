use crate::ast::NodeId;
use crate::hir::{Const, Enum, Function, Generics, HirId, Struct};
use crate::resolver::def::DefId;
use boron_source::ident_table::Identifier;
use boron_source::prelude::SourceFileId;
use dashmap::DashMap;
use dashmap::mapref::one::Ref;

#[derive(Debug)]
pub enum AdtEntry {
  Struct(Struct),
  Enum(Enum),
}

pub trait AdtItem {
  fn get_name(&self) -> Identifier;
  fn get_def_id(&self) -> DefId;
  fn get_generics(&self) -> Generics;
  fn has_item(&self, id: &DefId) -> bool;
}

impl AdtItem for Struct {
  fn get_name(&self) -> Identifier {
    self.name
  }
  fn get_def_id(&self) -> DefId {
    self.def_id
  }
  fn get_generics(&self) -> Generics {
    self.generics.clone()
  }
  fn has_item(&self, id: &DefId) -> bool {
    self.items.contains(id)
  }
}

impl AdtItem for Enum {
  fn get_name(&self) -> Identifier {
    self.name
  }
  fn get_def_id(&self) -> DefId {
    self.def_id
  }
  fn get_generics(&self) -> Generics {
    self.generics.clone()
  }
  fn has_item(&self, id: &DefId) -> bool {
    self.items.contains(id) || self.variants.iter().any(|v| &v.def_id == id)
  }
}

impl AdtItem for AdtEntry {
  fn get_name(&self) -> Identifier {
    match self {
      Self::Struct(s) => s.get_name(),
      Self::Enum(e) => e.get_name(),
    }
  }
  fn get_def_id(&self) -> DefId {
    match self {
      Self::Struct(s) => s.get_def_id(),
      Self::Enum(e) => e.get_def_id(),
    }
  }
  fn get_generics(&self) -> Generics {
    match self {
      Self::Struct(s) => s.get_generics(),
      Self::Enum(e) => e.get_generics(),
    }
  }

  fn has_item(&self, id: &DefId) -> bool {
    match self {
      Self::Struct(s) => s.has_item(id),
      Self::Enum(e) => e.has_item(id),
    }
  }
}

#[derive(Debug, Default)]
pub struct Hir {
  pub functions: DashMap<DefId, Function>,
  pub adts: DashMap<DefId, AdtEntry>,
  pub consts: DashMap<DefId, Const>,
  pub modules: DashMap<SourceFileId, Vec<DefId>>,
  pub node_to_hir: DashMap<NodeId, HirId>,
}

impl Hir {
  pub fn new() -> Self {
    Self::default()
  }

  pub fn get_function(&self, def_id: DefId) -> Option<Ref<'_, DefId, Function>> {
    self.functions.get(&def_id)
  }

  pub fn get_adt(&self, def_id: DefId) -> Option<Ref<'_, DefId, AdtEntry>> {
    self.adts.get(&def_id)
  }

  pub fn get_struct(&self, def_id: DefId) -> Option<Struct> {
    self.adts.get(&def_id).map(|adt| match adt.value() {
      AdtEntry::Struct(s) => s.clone(),
      AdtEntry::Enum(_) => panic!("expected Struct for {def_id:?}, found Enum"),
    })
  }

  pub fn get_enum(&self, def_id: DefId) -> Option<Enum> {
    self.adts.get(&def_id).map(|adt| match adt.value() {
      AdtEntry::Enum(e) => e.clone(),
      AdtEntry::Struct(_) => panic!("expected Enum for {def_id:?}, found Struct"),
    })
  }

  pub fn get_const(&self, def_id: DefId) -> Option<Ref<'_, DefId, Const>> {
    self.consts.get(&def_id)
  }

  pub fn get_module_items(
    &self,
    file_id: SourceFileId,
  ) -> Option<Ref<'_, SourceFileId, Vec<DefId>>> {
    self.modules.get(&file_id)
  }

  pub fn hir_to_node(&self, hir: &HirId) -> Option<NodeId> {
    self.node_to_hir.iter().find(|r| r.value() == hir).map(|r| *r.key())
  }

  pub fn find_adt_parent(&self, id: &DefId) -> Option<DefId> {
    self
      .adts
      .iter()
      .find(|entry| entry.value().has_item(id))
      .map(|entry| entry.value().get_def_id())
  }

  pub fn get_adt_generics(&self, id: &DefId) -> Option<Generics> {
    self.adts.get(id).map(|adt| adt.value().get_generics())
  }

  pub fn get_adt_name(&self, id: &DefId) -> Option<Identifier> {
    self.adts.get(id).map(|adt| adt.value().get_name())
  }
}

#[derive(Debug, Default)]
pub struct HirMap {
  nodes: DashMap<HirId, NodeData>,
  parents: DashMap<HirId, HirId>,
}

#[derive(Debug, Clone)]
pub enum NodeData {
  Function(DefId),
  Struct(DefId),
  Enum(DefId),
  Const(DefId),
  Nested { owner: DefId },
}

impl HirMap {
  pub fn new() -> Self {
    Self::default()
  }

  pub fn insert(&self, hir_id: HirId, data: NodeData) {
    self.nodes.insert(hir_id, data);
  }

  pub fn set_parent(&self, child: HirId, parent: HirId) {
    self.parents.insert(child, parent);
  }

  pub fn parent(&self, id: HirId) -> Option<HirId> {
    self.parents.get(&id).map(|r| *r)
  }

  pub fn owner(id: HirId) -> DefId {
    id.owner
  }

  pub fn get_node_data(&self, id: HirId) -> Option<NodeData> {
    self.nodes.get(&id).map(|r| r.clone())
  }
}
