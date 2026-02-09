use crate::llvm::LLVMCodegen;
use anyhow::Result;
use boron_analysis::literal_table::FullLiteral;
use boron_ir::{IrExpr, SemanticTy};
use boron_target::primitive::PrimitiveKind;
use inkwell::types::{BasicTypeEnum, StringRadix};
use inkwell::values::BasicValueEnum;

impl<'ctx> LLVMCodegen<'ctx> {
  pub fn build_literal(
    &self,
    expr: &IrExpr,
    lit: &FullLiteral,
  ) -> Result<BasicValueEnum<'ctx>> {
    let ty = self.ty(&expr.ty)?;

    match lit {
      FullLiteral::Bool(val) => {
        Ok(self.context.bool_type().const_int(u64::from(*val), false).into())
      }
      FullLiteral::Int(number) => {
        let BasicTypeEnum::IntType(i) = ty else { unreachable!() };

        Ok(
          self
            .require_some(
              i.const_int_from_string(&number.to_string(), StringRadix::Decimal),
              "invalid integer literal for LLVM",
            )?
            .into(),
        )
      }
      // TODO: might need to validate float before
      FullLiteral::Float(fl) => {
        let SemanticTy::Primitive(prim) = &expr.ty else {
          unreachable!("Float literal should have primitive type")
        };

        let float_str = fl.to_string();
        match prim {
          PrimitiveKind::F32 => unsafe {
            Ok(self.context.f32_type().const_float_from_string(&float_str).into())
          },
          PrimitiveKind::F64 => unsafe {
            Ok(self.context.f64_type().const_float_from_string(&float_str).into())
          },
          _ => unreachable!("Float literal should be f32 or f64"),
        }
      }
      FullLiteral::String(string) => {
        Ok(self.context.const_string(string.as_bytes(), false).into())
      }
      _ => todo!(),
    }
  }
}
