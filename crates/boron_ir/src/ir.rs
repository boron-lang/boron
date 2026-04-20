use crate::items::IrFunction;
use crate::{IrEnum, IrLocal, IrStruct};
use boron_resolver::DefId;
use boron_source::new_id;
use dashmap::DashMap;
use boron_types::hir::SemanticTy;

new_id!(IrId);

#[derive(Debug, Default, Clone)]
pub struct Ir {
  pub functions: Vec<IrFunction>,
  pub structs: Vec<IrStruct>,
  pub enums: Vec<IrEnum>,
  // all variables for current function
  pub locals: DashMap<IrId, Vec<IrLocal>>,
}

impl Ir {
  pub fn find_struct(&self, id: &DefId, types: &Vec<SemanticTy>) -> &IrStruct {
    let strukt = self.structs.iter().find(|s| &s.def_id == id && types == &s.type_args);

    if let Some(strukt) = strukt {
      strukt
    } else {
      panic!("couldn't find struct for {id:?} {types:#?}")
    }
  }

  pub fn find_enum(&self, id: &DefId, types: &Vec<SemanticTy>) -> &IrEnum {
    let _enum = self.enums.iter().find(|s| &s.def_id == id && types == &s.type_args);
    if let Some(_enum) = _enum {
      _enum
    } else {
      panic!("couldn't find enum for {id:?} {types:#?}")
    }
  }

  pub fn get_enum(&self, id: &DefId, types: &Vec<SemanticTy>) -> Option<&IrEnum> {
    self.enums.iter().find(|s| &s.def_id == id && types == &s.type_args)
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
      x.push(local);
    } else {
      self.locals.insert(id, vec![local]);
    }
  }
}
