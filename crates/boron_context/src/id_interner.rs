use boron_source::{PackageId, StablePackageId};
use dashmap::DashMap;

#[derive(Default, Debug)]
pub struct IdInterner {
  pub stable_package_id: DashMap<StablePackageId, PackageId>,
}

impl IdInterner {
  pub fn intern_package_id(&self, id: StablePackageId) -> PackageId {
    *self.stable_package_id.entry(id).or_insert_with(|| PackageId::new())
  }
}
