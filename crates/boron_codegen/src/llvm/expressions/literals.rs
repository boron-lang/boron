use crate::llvm::LLVMCodegen;
use boron_analysis::literal_table::FullLiteral;
use boron_ir::{IrExpr, SemanticTy};
use boron_target::primitive::PrimitiveKind;
use inkwell::types::{BasicTypeEnum, StringRadix};
use inkwell::values::BasicValueEnum;

impl<'ctx> LLVMCodegen<'ctx> {
  pub fn build_literal(&self, expr: &IrExpr, lit: &FullLiteral) -> BasicValueEnum<'ctx> {
    let ty = self.ty(&expr.ty);

    match lit {
      FullLiteral::Bool(val) => {
        self.context.bool_type().const_int(if *val { 1 } else { 0 }, false).into()
      }
      FullLiteral::Int(number) => {
        let BasicTypeEnum::IntType(i) = ty else { unreachable!() };

        i.const_int_from_string(&number.to_string(), StringRadix::Decimal)
          .expect("couldn't creat llvm int literal")
          .into()
      }
      // TODO: might need to validate float before
      FullLiteral::Float(fl) => {
        let SemanticTy::Primitive(prim) = &expr.ty else {
          unreachable!("Float literal should have primitive type")
        };

        let float_str = fl.to_string();

        match prim {
          PrimitiveKind::F32 => unsafe {
            self.context.f32_type().const_float_from_string(&float_str).into()
          },
          PrimitiveKind::F64 => unsafe {
            self.context.f64_type().const_float_from_string(&float_str).into()
          },
          _ => unreachable!("Float literal should be f32 or f64"),
        }
      }
      _ => todo!(),
    }
  }
}
