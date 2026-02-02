use crate::items::IrFunction;
use crate::IrStruct;
use boron_source::new_id;

#[derive(Debug, Default, Clone)]
pub struct Ir {
  pub functions: Vec<IrFunction>,
  pub structs: Vec<IrStruct>,
}

new_id!(IrId);
