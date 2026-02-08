mod llvm;

use crate::llvm::LLVMCodegen;
use boron_ir::Ir;
use boron_utils::context::Context;
use dashmap::DashMap;
use inkwell::context::Context as LLVMContext;
use inkwell::targets::{
  CodeModel, InitializationConfig, RelocMode, Target, TargetTriple,
};
use inkwell::OptimizationLevel;

pub trait Codegen {
  fn backend_name(&self) -> &str;

  fn generate(&self, ir: &Ir) -> anyhow::Result<()>;
}

pub fn run_codegen<'a>(ctx: &'a Context<'a>, ir: &Ir) -> anyhow::Result<()> {
  Target::initialize_native(&InitializationConfig::default())
    .expect("failed to init native target");

  let triple = TargetTriple::create(ctx.target().triple);
  let target = Target::from_triple(&triple).expect("failed to get target");

  let target_machine = target
    .create_target_machine(
      &triple,
      "generic",
      "",
      OptimizationLevel::None,
      RelocMode::Default,
      CodeModel::Default,
    )
    .expect("failed to create target machine");

  let target_data = target_machine.get_target_data();
  let llvm_ctx = LLVMContext::create();
  let module = llvm_ctx.create_module("program");

  module.set_data_layout(&target_data.get_data_layout());
  module.set_triple(&triple);

  let codegen = LLVMCodegen {
    ctx,
    context: &llvm_ctx,
    module,
    builder: llvm_ctx.create_builder(),
    target_machine,
    structs: DashMap::new(),
    funcs: DashMap::new(),
    locals: DashMap::new(),
    ir,
  };

  codegen.generate(ir)
}
