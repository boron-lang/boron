use dashmap::DashMap;
use crate::{IrLocal, IrStruct};
use crate::items::IrFunction;
use boron_hir::{HirId, SemanticTy};
use boron_resolver::DefId;
use boron_source::new_id;

new_id!(IrId);

#[derive(Debug, Default, Clone)]
pub struct Ir {
  pub functions: Vec<IrFunction>,
  pub structs: Vec<IrStruct>,
  // all variables for current function
  pub locals: DashMap<IrId, Vec<IrLocal>>
}

impl Ir {
  pub fn find_struct(&self, id: &DefId, types: &Vec<SemanticTy>) -> &IrStruct {
    let strukt = self
        .structs
        .iter()
        .find(|s| &s.def_id == id && types == &s.type_args);

    if let Some(strukt) = strukt {
      strukt 
    } else {
      panic!("couldn't find struct for {:?} {:#?}", id, types)
    }
  }

  pub fn find_function(&self, id: &DefId, types: &Vec<SemanticTy>) -> &IrFunction {
    self
      .functions
      .iter()
      .find(|s| &s.def_id == id && types == &s.type_args)
      .expect("all functions should be known")
  }
  
  pub fn add_local(&self, id: IrId, local: IrLocal) {
    if let Some(ref mut x) = self.locals.get_mut(&id) {
      x.push(local)
    } else {
      self.locals.insert(id, vec![local]);
    }
  }
}
