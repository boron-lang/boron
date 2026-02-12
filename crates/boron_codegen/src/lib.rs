mod llvm;

use crate::llvm::LLVMCodegen;
use anyhow::Result;
use boron_ir::Ir;
use boron_resolver::DefId;
use boron_session::prelude::Session;
use dashmap::DashMap;
use inkwell::context::Context as LLVMContext;

pub trait Codegen {
  fn backend_name(&self) -> &str;

  fn generate(&self, ir: &Ir) -> Result<()>;
}

pub fn run_codegen(sess: &Session, ir: &Ir, main_function: Option<DefId>) -> Result<()> {
  let llvm_ctx = LLVMContext::create();
  let module = llvm_ctx.create_module("program");

  let target_data = sess.target().target_machine.get_target_data();
  module.set_data_layout(&target_data.get_data_layout());
  module.set_triple(&sess.target().triple);

  let codegen = LLVMCodegen {
    sess,
    context: &llvm_ctx,
    module,
    builder: llvm_ctx.create_builder(),
    structs: DashMap::new(),
    funcs: DashMap::new(),
    locals: DashMap::new(),
    struct_init_allocs: DashMap::new(),
    ir,
  };

  codegen.generate(ir, &main_function)
}
