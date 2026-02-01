use crate::items::IrFunction;
use boron_source::new_id;

mod items;
mod lowerer;
mod mangler;

pub use items::*;
pub use lowerer::*;
pub use mangler::*;

#[derive(Debug, Default, Clone)]
pub struct Ir {
  pub functions: Vec<IrFunction>,
  pub structs: Vec<IrStruct>,
}

new_id!(IrId);
