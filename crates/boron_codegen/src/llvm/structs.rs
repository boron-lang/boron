use crate::llvm::LLVMCodegen;
use boron_ir::IrStruct;

impl LLVMCodegen<'_> {
  pub fn generate_struct(&self, strukt: &IrStruct) {
    let ty = self.context.opaque_struct_type(&strukt.name);
  }
}
