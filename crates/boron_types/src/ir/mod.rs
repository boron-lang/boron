use crate::hir::SemanticTy;
use crate::ir::exprs::IrLocal;
use crate::ir::items::{IrEnum, IrFunction, IrStruct};
use boron_source::{new_id, DefId};
use dashmap::DashMap;

pub mod exprs;
pub mod items;

new_id!(IrId);

#[derive(Debug, Default, Clone)]
pub struct Ir {
  pub functions: DashMap<DefId, IrFunction>,
  pub structs: DashMap<DefId, IrStruct>,
  pub enums: DashMap<DefId, IrEnum>,
  // all variables for current function
  pub locals: DashMap<IrId, Vec<IrLocal>>,
}

impl Ir {
  pub fn find_struct(&self, id: &DefId, types: &Vec<SemanticTy>) -> IrStruct {
    self.structs
      .iter()
      .find(|s| &s.def_id == id && types == &s.type_args)
      .map(|s| s.clone())
      .unwrap_or_else(|| panic!("couldn't find struct for {id:?} {types:#?}"))
  }

  pub fn find_enum(&self, id: &DefId, types: &Vec<SemanticTy>) -> IrEnum {
    self.enums
      .iter()
      .find(|s| &s.def_id == id && types == &s.type_args)
      .map(|e| e.clone())
      .unwrap_or_else(|| panic!("couldn't find enum for {id:?} {types:#?}"))
  }

  pub fn get_enum(&self, id: &DefId, types: &Vec<SemanticTy>) -> Option<IrEnum> {
    self.enums
      .iter()
      .find(|s| &s.def_id == id && types == &s.type_args)
      .map(|e| e.clone())
  }

  pub fn find_function(&self, id: &DefId, types: &Vec<SemanticTy>) -> IrFunction {
    self.functions
      .iter()
      .find(|s| &s.def_id == id && types == &s.type_args)
      .map(|f| f.clone())
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