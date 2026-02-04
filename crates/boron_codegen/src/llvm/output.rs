use crate::llvm::LLVMCodegen;
use std::fs;

impl LLVMCodegen<'_> {
  pub fn output_ir(&self) {
    let module = self.module.print_to_string().to_string();

    let config = self.ctx.session.config();
    let output_dir = config.output.join("ir");
    fs::create_dir_all(&output_dir).expect("can't create");
    let output_path = output_dir.join(format!("{}.ll", config.name));

    if fs::write(&output_path, module).is_ok() {}
  }
}
