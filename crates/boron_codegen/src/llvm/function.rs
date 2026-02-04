use crate::llvm::LLVMCodegen;
use crate::llvm::blocks::{BlockContext, BlockGeneratorContext};
use boron_ir::{IrFunction, IrId, SemanticTy};
use boron_resolver::DefId;
use inkwell::module::Linkage;
use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType};
use inkwell::values::FunctionValue;
use itertools::Itertools;

impl<'ctx> LLVMCodegen<'ctx> {
  fn params_to_metadata(
    params: &[BasicTypeEnum<'ctx>],
  ) -> Vec<BasicMetadataTypeEnum<'ctx>> {
    params.iter().map(|p| (*p).into()).collect()
  }

  pub fn fn_ret_ty(
    &self,
    ret: &SemanticTy,
    params: &[BasicTypeEnum<'ctx>],
    varargs: bool,
  ) -> FunctionType<'ctx> {
    let params = Self::params_to_metadata(params);

    match ret {
      SemanticTy::Unit => self.context.void_type().fn_type(&params, varargs),
      _ => self.ty(ret).fn_type(&params, varargs),
    }
  }

  pub fn function_value(&self, id: &IrId) -> FunctionValue<'ctx> {
    self.funcs.get(id).expect("should exist").clone()
  }

  pub fn create_function_body(&self, func: &IrFunction) {
    let params = func.params.iter().map(|param| self.ty(&param.ty)).collect_vec();
    let fn_ty = self.fn_ret_ty(&func.return_type, &params, false);

    let linkage = if func.body.is_some() { None } else { Some(Linkage::External) };
    let function = self.module.add_function(&func.name, fn_ty, linkage);

    self.funcs.insert(func.id, function);
  }

  pub fn generate_function_body(&self, func: &IrFunction) {
    let function = self.function_value(&func.id);
    self.locals.clear();

    if let Some(body) = &func.body {
      for block in &body.blocks {
        let context = if block.hir_id == body.entry {
          BlockContext::FunctionStart
        } else {
          BlockContext::Normal
        };

        self.generate_block(&BlockGeneratorContext::new(block, func, function, context));
      }
    }
  }
}
