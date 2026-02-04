mod blocks;
mod function;
mod output;
mod structs;
mod types;

use crate::Codegen;
use boron_ir::{Ir, IrId};
use boron_utils::context::Context;
use dashmap::DashMap;
use inkwell::builder::Builder;
use inkwell::context::Context as LLVMContext;
use inkwell::module::Module;
use inkwell::targets::TargetData;
use inkwell::types::StructType;

pub struct LLVMCodegen<'ctx> {
  pub ctx: &'ctx Context<'ctx>,
  pub context: &'ctx LLVMContext,
  pub module: Module<'ctx>,
  pub builder: Builder<'ctx>,
  pub target_data: TargetData,
  pub structs: DashMap<IrId, StructType<'ctx>>,
  pub ir: &'ctx Ir,
}

impl<'ctx> Codegen for LLVMCodegen<'ctx> {
  fn backend_name(&self) -> &'static str {
    "LLVM"
  }

  fn generate(&self, ir: &Ir) {
    for strukt in &ir.structs {
      self.create_struct_type(strukt);
    }

    for strukt in &ir.structs {
      self.fill_struct_bodies(strukt);
    }

    for func in &ir.functions {
      self.generate_function(&func);
    }

    self.output_ir();
  }
}
