use crate::ty::SubstitutionMap;
use boron_resolver::DefId;

#[derive(Debug, Clone)]
pub struct MonomorphizationEntry {
  pub def_id: DefId,
  pub type_args: SubstitutionMap,
}
