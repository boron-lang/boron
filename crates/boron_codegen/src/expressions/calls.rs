use crate::codegen::LLVMCodegen;
use crate::expressions::ValueKind;
use anyhow::Result;
use boron_ir::{IrExpr, SemanticTy};
use boron_resolver::DefId;
use boron_session::prelude::Identifier;
use inkwell::values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum};

impl<'ctx> LLVMCodegen<'ctx> {
  fn args_to_metadata(
    params: &[BasicValueEnum<'ctx>],
  ) -> Vec<BasicMetadataValueEnum<'ctx>> {
    params.iter().map(|p| (*p).into()).collect()
  }

  pub fn generate_call(
    &self,
    callee: &DefId,
    callee_name: &Identifier,
    type_args: &Vec<SemanticTy>,
    args: &Vec<IrExpr>,
  ) -> Result<ValueKind<'ctx>> {
    if let Some(enum_) = self.ir.get_enum(callee, type_args) {
      return self.build_enum_tuple(enum_, callee_name, args);
    }

    let ir_function = self.ir.find_function(callee, type_args);
    let function = self.function_value(&ir_function.id)?;
    let mut largs = vec![];

    for arg in args {
      largs.push(self.value_to_basic(self.ty(&arg.ty)?, self.generate_expr(arg)?)?);
    }

    let call_site = self.require_llvm(
      self.builder.build_call(
        function,
        &Self::args_to_metadata(largs.as_slice()),
        &format!("call.{}", ir_function.name),
      ),
      "build call",
    )?;

    match call_site.try_as_basic_value().basic() {
      Some(val) => Ok(val.into()),
      None => {
        Ok(self.context.i32_type().const_int(0, false).as_basic_value_enum().into())
      }
    }
  }
}
