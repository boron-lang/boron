use crate::items::IrFunction;
use crate::IrStruct;
use boron_hir::SemanticTy;
use boron_resolver::DefId;
use boron_source::new_id;

new_id!(IrId);

#[derive(Debug, Default, Clone)]
pub struct Ir {
  pub functions: Vec<IrFunction>,
  pub structs: Vec<IrStruct>,
}

impl Ir {
  pub fn find_struct(&self, id: &DefId, types: &Vec<SemanticTy>) -> &IrStruct {
    self
      .structs
      .iter()
      .find(|s| &s.def_id == id && types == &s.type_args)
      .expect("all structs should be known")
  }
  
  pub fn find_function(&self, id: &DefId, types: &Vec<SemanticTy>) -> &IrFunction {
    self
        .functions
        .iter()
        .find(|s| &s.def_id == id && types == &s.type_args)
        .expect("all functions should be known")
  }
}
