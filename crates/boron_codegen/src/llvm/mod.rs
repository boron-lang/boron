mod blocks;
mod expressions;
mod function;
mod output;
mod structs;
mod types;

use crate::Codegen;
use anyhow::{Result, anyhow};
use boron_ir::{Ir, IrId};
use boron_resolver::DefId;
use boron_utils::context::Context;
use boron_utils::prelude::Mode;
use dashmap::DashMap;
use inkwell::builder::Builder;
use inkwell::context::Context as LLVMContext;
use inkwell::module::Module;
use inkwell::passes::PassBuilderOptions;
use inkwell::targets::TargetMachine;
use inkwell::types::StructType;
use inkwell::values::{FunctionValue, PointerValue};

pub struct LLVMCodegen<'ctx> {
  pub ctx: &'ctx Context<'ctx>,
  pub context: &'ctx LLVMContext,
  pub module: Module<'ctx>,
  pub builder: Builder<'ctx>,
  pub target_machine: TargetMachine,
  pub structs: DashMap<IrId, StructType<'ctx>>,
  pub funcs: DashMap<IrId, FunctionValue<'ctx>>,
  pub locals: DashMap<DefId, PointerValue<'ctx>>,
  pub ir: &'ctx Ir,
}

impl<'ctx> Codegen for LLVMCodegen<'ctx> {
  fn backend_name(&self) -> &'static str {
    "LLVM"
  }

  fn generate(&self, ir: &Ir) -> Result<()> {
    for strukt in &ir.structs {
      self.create_struct_type(strukt);
    }

    for strukt in &ir.structs {
      self.generate_struct_body(strukt);
    }

    for func in &ir.functions {
      self.create_function_body(&func);
    }

    for func in &ir.functions {
      self.generate_function_body(&func)
    }

    let opt_level = if self.ctx.session.config().mode == Mode::Release {
      "default<O3>"
    } else {
      "default<O0>"
    };
    self
      .module
      .run_passes(opt_level, &self.target_machine, PassBuilderOptions::create())
      .map_err(|s| anyhow!("Failed to run optimization passes: {}", s))?;

    self.output_ir()?;
    Ok(())
  }
}
