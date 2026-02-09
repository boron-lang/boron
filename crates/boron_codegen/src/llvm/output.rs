use crate::llvm::LLVMCodegen;
use anyhow::{Result, anyhow};
use inkwell::targets::FileType;

impl LLVMCodegen<'_> {
  pub fn output_ir(&self) -> Result<()> {
    let config = self.ctx.session.config();

    let module = self.module.print_to_string().to_string();
    let output_dir = config.output.join("ir");
    fs_err::create_dir_all(&output_dir)?;
    let output_path = output_dir.join(format!("{}.ll", config.name));
    fs_err::write(&output_path, module)?;

    let obj_dir = config.output.join("obj");
    fs_err::create_dir_all(&obj_dir)?;

    self
      .target_machine
      .write_to_file(
        &self.module,
        FileType::Object,
        &obj_dir.join(format!("{}{}", config.name, self.ctx.target().obj_file_suffix())),
      )
      .map_err(|e| anyhow!("Failed to write object file {e}"))
  }
}
