mod blocks;
mod expressions;
mod function;
mod output;
mod structs;
mod types;

use std::fmt::Display;
use crate::Codegen;
use anyhow::{anyhow, Result};
use boron_ir::{Ir, IrId};
use boron_resolver::DefId;
use boron_utils::prelude::{Mode, Session};
use dashmap::DashMap;
use inkwell::builder::Builder;
use inkwell::context::Context as LLVMContext;
use inkwell::module::Module;
use inkwell::passes::PassBuilderOptions;
use inkwell::targets::TargetMachine;
use inkwell::types::StructType;
use inkwell::values::{FunctionValue, PointerValue};

pub struct LLVMCodegen<'ctx> {
  pub sess: &'ctx Session,
  pub context: &'ctx LLVMContext,
  pub module: Module<'ctx>,
  pub builder: Builder<'ctx>,
  pub target_machine: TargetMachine,
  pub structs: DashMap<IrId, StructType<'ctx>>,
  pub funcs: DashMap<IrId, FunctionValue<'ctx>>,
  pub locals: DashMap<DefId, PointerValue<'ctx>>,
  pub struct_init_allocs: DashMap<IrId, PointerValue<'ctx>>,
  pub ir: &'ctx Ir,
}

impl Codegen for LLVMCodegen<'_> {
  fn backend_name(&self) -> &'static str {
    "LLVM"
  }

  fn generate(&self, ir: &Ir) -> Result<()> {
    for strukt in &ir.structs {
      self.create_struct_type(strukt);
    }
    for strukt in &ir.structs {
      self.generate_struct_body(strukt)?;
    }

    for func in &ir.functions {
      self.create_function_body(func)?;
    }
    for func in &ir.functions {
      self.generate_function_body(func)?;
    }

    let opt_level = if self.sess.config().mode == Mode::Release {
      "default<O3>"
    } else {
      "default<O0>"
    };
    self
      .module
      .run_passes(opt_level, &self.target_machine, PassBuilderOptions::create())
      .map_err(|s| anyhow!("Failed to run optimization passes: {s}"))?;

    self.output_ir()?;
    Ok(())
  }
}

impl<'ctx> LLVMCodegen<'ctx> {
  pub fn require_some<T>(&self, value: Option<T>, context: &str) -> Result<T> {
    value.ok_or_else(|| anyhow!("LLVM codegen invariant violated: {context}"))
  }

  pub fn require_llvm<T, E: Display>(
    &self,
    value: Result<T, E>,
    context: &str,
  ) -> Result<T> {
    value.map_err(|err| anyhow!("LLVM codegen failed ({context}): {err}"))
  }

  pub fn struct_ty_by_id(&self, id: &IrId) -> Result<StructType<'ctx>> {
    let strukt = self.require_some(self.structs.get(id), "struct type missing")?;
    Ok(*strukt.value())
  }
}
