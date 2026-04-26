use crate::new_id;
use serde::{Deserialize, Serialize};
use std::fmt::{Display, Formatter};
use std::hash::{DefaultHasher, Hash, Hasher};
use tracing::debug;

#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq)]
pub struct DefId {
  pub def_index: DefIndex,
  pub package_index: PackageId,
}

impl DefId {
  pub fn new(package_index: PackageId) -> Self {
    Self { def_index: DefIndex::new(), package_index }
  }
}

impl Display for DefId {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}_{}", self.package_index, self.def_index)
  }
}

new_id!(DefIndex);
new_id!(PackageId);

/// Stable PackageId that is hashed from package_name, version and the package's type.
#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq, Serialize, Deserialize)]
pub struct StablePackageId(pub u64);

impl StablePackageId {
  pub fn new(package_name: String, version: String, is_exe: bool) -> Self {
    let mut hasher = DefaultHasher::new();
    package_name.hash(&mut hasher);
    version.hash(&mut hasher);

    hasher.write(if is_exe { b"exe" } else { b"lib" });
    let id = hasher.finish();
    debug!(id, package_name, version, is_exe, "StablePackageId created");
    Self(id)
  }
}

/// Stable definition id that consists of StablePackageId and path to the definition (foo::Bar)
#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq, Serialize, Deserialize)]
pub struct StableDefId(pub u64, pub u64);

impl StableDefId {
  pub fn new(stable_package_id: StablePackageId, path: Vec<String>) -> Self {
    let mut hasher = DefaultHasher::new();
    path.hash(&mut hasher);

    let id = hasher.finish();
    debug!(id, ?path, "StableDefId created");

    Self(stable_package_id.0, id)
  }
}
