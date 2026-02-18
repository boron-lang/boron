use crate::codegen::LLVMCodegen;
use anyhow::Result;
use boron_ir::{IrExpr, SemanticTy};
use boron_parser::BinaryOp;
use inkwell::values::BasicValueEnum;
use inkwell::FloatPredicate;
use inkwell::IntPredicate;

impl<'ctx> LLVMCodegen<'ctx> {
  pub fn generate_binary_op(
    &self,
    lhs: &IrExpr,
    op: &BinaryOp,
    rhs: &IrExpr,
  ) -> Result<BasicValueEnum<'ctx>> {
    match (&lhs.ty, &rhs.ty) {
      (SemanticTy::Primitive(lhs_ty), SemanticTy::Primitive(rhs_ty)) => {
        if lhs_ty.is_integer() && rhs_ty.is_integer() {
          let lhs_val = self
            .value_to_basic(self.primitive_ty(lhs_ty), self.generate_expr(lhs)?)?
            .into_int_value();
          let rhs_val = self
            .value_to_basic(self.primitive_ty(rhs_ty), self.generate_expr(rhs)?)?
            .into_int_value();

          macro_rules! int_op {
            ($method:ident, $name:expr) => {
              Ok(
                self
                  .require_llvm(self.builder.$method(lhs_val, rhs_val, $name), $name)?
                  .into(),
              )
            };
          }

          macro_rules! int_cmp {
            ($pred:ident, $name:expr) => {{
              let value = self.require_llvm(
                self.builder.build_int_compare(
                  IntPredicate::$pred,
                  lhs_val,
                  rhs_val,
                  $name,
                ),
                $name,
              )?;
              Ok(value.into())
            }};
          }

          match op {
            BinaryOp::Add => int_op!(build_int_add, "int.add"),
            BinaryOp::Sub => int_op!(build_int_sub, "int.sub"),
            BinaryOp::Mul => int_op!(build_int_mul, "int.mul"),
            BinaryOp::Div => int_op!(build_int_signed_div, "int.div"),
            BinaryOp::Mod => int_op!(build_int_signed_rem, "int.mod"),

            BinaryOp::Eq => int_cmp!(EQ, "int.eq"),
            BinaryOp::Ne => int_cmp!(NE, "int.ne"),
            BinaryOp::Lt => int_cmp!(SLT, "int.lt"),
            BinaryOp::Le => int_cmp!(SLE, "int.le"),
            BinaryOp::Gt => int_cmp!(SGT, "int.gt"),
            BinaryOp::Ge => int_cmp!(SGE, "int.ge"),

            BinaryOp::And => int_op!(build_and, "int.and"),
            BinaryOp::Or => int_op!(build_or, "int.or"),

            BinaryOp::BitAnd => int_op!(build_and, "int.bitand"),
            BinaryOp::BitOr => int_op!(build_or, "int.bitor"),
            BinaryOp::BitXor => int_op!(build_xor, "int.bitxor"),
            BinaryOp::Shl => int_op!(build_left_shift, "int.shl"),
            BinaryOp::Shr => {
              let value = self.require_llvm(
                self.builder.build_right_shift(lhs_val, rhs_val, true, "int.shr"),
                "int.shr",
              )?;
              Ok(value.into())
            }
          }
        } else if lhs_ty.is_float() && rhs_ty.is_float() {
          let lhs_val = self
            .value_to_basic(self.primitive_ty(lhs_ty), self.generate_expr(lhs)?)?
            .into_float_value();
          let rhs_val = self
            .value_to_basic(self.primitive_ty(rhs_ty), self.generate_expr(rhs)?)?
            .into_float_value();

          macro_rules! float_op {
            ($method:ident, $name:expr) => {
              Ok(
                self
                  .require_llvm(self.builder.$method(lhs_val, rhs_val, $name), $name)?
                  .into(),
              )
            };
          }

          macro_rules! float_cmp {
            ($pred:ident, $name:expr) => {{
              let value = self.require_llvm(
                self.builder.build_float_compare(
                  FloatPredicate::$pred,
                  lhs_val,
                  rhs_val,
                  $name,
                ),
                $name,
              )?;
              Ok(value.into())
            }};
          }

          match op {
            BinaryOp::Add => float_op!(build_float_add, "float.add"),
            BinaryOp::Sub => float_op!(build_float_sub, "float.sub"),
            BinaryOp::Mul => float_op!(build_float_mul, "float.mul"),
            BinaryOp::Div => float_op!(build_float_div, "float.div"),
            BinaryOp::Mod => float_op!(build_float_rem, "float.mod"),

            BinaryOp::Eq => float_cmp!(OEQ, "float.eq"),
            BinaryOp::Ne => float_cmp!(ONE, "float.ne"),
            BinaryOp::Lt => float_cmp!(OLT, "float.lt"),
            BinaryOp::Le => float_cmp!(OLE, "float.le"),
            BinaryOp::Gt => float_cmp!(OGT, "float.gt"),
            BinaryOp::Ge => float_cmp!(OGE, "float.ge"),

            _ => unreachable!("Operation {:?} not supported for floats", op),
          }
        } else {
          unreachable!("Mixed integer/float operations not yet implemented")
        }
      }
      _ => unreachable!("{:?} {} {:?}", lhs, op, rhs),
    }
  }
}
