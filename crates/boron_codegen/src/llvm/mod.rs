mod output;
mod structs;

use crate::Codegen;
use boron_ir::Ir;
use boron_utils::context::Context;
use inkwell::builder::Builder;
use inkwell::context::Context as LLVMContext;
use inkwell::module::Module;

pub struct LLVMCodegen<'a> {
  pub ctx: &'a Context<'a>,
  pub context: &'a LLVMContext,
  pub module: Module<'a>,
  pub builder: Builder<'a>,
}

impl<'a> Codegen for LLVMCodegen<'a> {
  fn backend_name(&self) -> &str {
    "LLVM"
  }

  fn generate(&self, ir: &Ir) {
    for strukt in &ir.structs {
      self.generate_struct(&strukt);
    }

    self.output_ir();
  }
}
