use crate::interpreter::values::ConstValue;
use dashmap::DashMap;
use zirael_hir::{Expr, HirId};
use zirael_parser::NodeId;

#[derive(Debug)]
pub struct BuiltInResults(DashMap<HirId, ConstValue>);

impl BuiltInResults {
  pub fn new() -> Self {
    Self(DashMap::new())
  }

  pub fn map(&self) -> &DashMap<HirId, ConstValue> {
    &self.0
  }

  pub fn insert(&self, id: HirId, result: ConstValue) {
    self.map().insert(id, result);
  }

  pub fn get(&self, id: HirId) -> Option<ConstValue> {
    self.map().get(&id).map(|v| v.clone())
  }
}
