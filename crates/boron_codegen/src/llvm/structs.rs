use crate::llvm::LLVMCodegen;
use boron_ir::IrStruct;

impl<'a> LLVMCodegen<'a> {
  pub fn generate_struct(&self, strukt: &IrStruct) {
    let ty = self.context.opaque_struct_type(&strukt.name);
  }
}
