mod llvm;

use crate::llvm::LLVMCodegen;
use boron_ir::Ir;
use boron_utils::context::Context;
use inkwell::context::Context as LLVMContext;
use std::io::Write;

pub trait Codegen {
  fn backend_name(&self) -> &str;

  fn generate(&self, ir: &Ir);
}

pub fn run_codegen<'a>(ctx: &'a Context<'a>, ir: &Ir) {
  let llvm_ctx = LLVMContext::create();

  let codegen = LLVMCodegen {
    ctx,
    context: &llvm_ctx,
    module: llvm_ctx.create_module("program"),
    builder: llvm_ctx.create_builder(),
  };

  codegen.generate(ir);
}
