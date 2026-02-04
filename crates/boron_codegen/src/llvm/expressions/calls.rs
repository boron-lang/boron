use crate::llvm::LLVMCodegen;
use boron_ir::{IrExpr, SemanticTy};
use boron_resolver::DefId;
use inkwell::values::{BasicMetadataValueEnum, BasicValueEnum};

impl<'ctx> LLVMCodegen<'ctx> {
  fn args_to_metadata(
    params: &[BasicValueEnum<'ctx>],
  ) -> Vec<BasicMetadataValueEnum<'ctx>> {
    params.iter().map(|p| (*p).into()).collect()
  }

  pub fn generate_call(
    &self,
    callee: &DefId,
    type_args: &Vec<SemanticTy>,
    args: &Vec<IrExpr>,
  ) -> BasicValueEnum<'ctx> {
    let ir_function = self.ir.find_function(callee, type_args);
    let function = self.function_value(&ir_function.id);
    let mut largs = vec![];

    for arg in args {
      largs.push(self.generate_expr(&arg))
    }

    let call_site = self
      .builder
      .build_call(function, &Self::args_to_metadata(largs.as_slice()), &ir_function.name)
      .unwrap();

    match call_site.try_as_basic_value().basic() {
      Some(val) => val,
      None => self.context.i32_type().const_int(0, false).into(),
    }
  }
}
