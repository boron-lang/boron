use crate::llvm::LLVMCodegen;

impl LLVMCodegen<'_> {
  pub fn output_ir(&self) {
    let module = self.module.print_to_string().to_string();
    println!("{:?}", module);
  }
}
