use boron_source::{PackageId, StablePackageId};
use dashmap::DashMap;

#[derive(Default, Debug)]
pub struct IdInterner {
  pub stable_package_id: DashMap<StablePackageId, PackageId>,
  pub package_id_to_stable: DashMap<PackageId, StablePackageId>,
}

impl IdInterner {
  pub fn intern_package_id(&self, id: StablePackageId) -> PackageId {
    let pkg = *self.stable_package_id.entry(id).or_insert_with(|| PackageId::new());
    self.package_id_to_stable.insert(pkg, id);
    pkg
  }

  pub fn stable_package_id(&self, id: PackageId) -> Option<StablePackageId> {
    self.package_id_to_stable.get(&id).map(|stable_id| *stable_id)
  }
}
