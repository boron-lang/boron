use boron_resolver::DefId;
use boron_types::infer_ty::SubstitutionMap;

#[derive(Debug, Clone)]
pub struct MonomorphizationEntry {
  pub def_id: DefId,
  pub type_args: SubstitutionMap,
}
