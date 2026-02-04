use crate::llvm::LLVMCodegen;
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
  ) -> BasicValueEnum<'ctx> {
    match (&lhs.ty, &rhs.ty) {
      (SemanticTy::Primitive(lhs_ty), SemanticTy::Primitive(rhs_ty)) => {
        if lhs_ty.is_integer() && rhs_ty.is_integer() {
          let lhs_val = self.generate_expr(lhs).into_int_value();
          let rhs_val = self.generate_expr(rhs).into_int_value();

          macro_rules! int_op {
            ($method:ident, $name:expr) => {
              self.builder.$method(lhs_val, rhs_val, $name).unwrap().into()
            };
          }

          macro_rules! int_cmp {
            ($pred:ident, $name:expr) => {
              self
                .builder
                .build_int_compare(IntPredicate::$pred, lhs_val, rhs_val, $name)
                .unwrap()
                .into()
            };
          }

          match op {
            BinaryOp::Add => int_op!(build_int_add, "add"),
            BinaryOp::Sub => int_op!(build_int_sub, "sub"),
            BinaryOp::Mul => int_op!(build_int_mul, "mul"),
            BinaryOp::Div => int_op!(build_int_signed_div, "div"),
            BinaryOp::Mod => int_op!(build_int_signed_rem, "mod"),

            BinaryOp::Eq => int_cmp!(EQ, "eq"),
            BinaryOp::Ne => int_cmp!(NE, "ne"),
            BinaryOp::Lt => int_cmp!(SLT, "lt"),
            BinaryOp::Le => int_cmp!(SLE, "le"),
            BinaryOp::Gt => int_cmp!(SGT, "gt"),
            BinaryOp::Ge => int_cmp!(SGE, "ge"),

            BinaryOp::And => int_op!(build_and, "and"),
            BinaryOp::Or => int_op!(build_or, "or"),

            BinaryOp::BitAnd => int_op!(build_and, "bitand"),
            BinaryOp::BitOr => int_op!(build_or, "bitor"),
            BinaryOp::BitXor => int_op!(build_xor, "bitxor"),
            BinaryOp::Shl => int_op!(build_left_shift, "shl"),
            BinaryOp::Shr => self
              .builder
              .build_right_shift(lhs_val, rhs_val, true, "shr")
              .unwrap()
              .into(),
          }
        } else if lhs_ty.is_float() && rhs_ty.is_float() {
          let lhs_val = self.generate_expr(lhs).into_float_value();
          let rhs_val = self.generate_expr(rhs).into_float_value();

          macro_rules! float_op {
            ($method:ident, $name:expr) => {
              self.builder.$method(lhs_val, rhs_val, $name).unwrap().into()
            };
          }

          macro_rules! float_cmp {
            ($pred:ident, $name:expr) => {
              self
                .builder
                .build_float_compare(FloatPredicate::$pred, lhs_val, rhs_val, $name)
                .unwrap()
                .into()
            };
          }

          match op {
            BinaryOp::Add => float_op!(build_float_add, "add"),
            BinaryOp::Sub => float_op!(build_float_sub, "sub"),
            BinaryOp::Mul => float_op!(build_float_mul, "mul"),
            BinaryOp::Div => float_op!(build_float_div, "div"),
            BinaryOp::Mod => float_op!(build_float_rem, "mod"),

            BinaryOp::Eq => float_cmp!(OEQ, "eq"),
            BinaryOp::Ne => float_cmp!(ONE, "ne"),
            BinaryOp::Lt => float_cmp!(OLT, "lt"),
            BinaryOp::Le => float_cmp!(OLE, "le"),
            BinaryOp::Gt => float_cmp!(OGT, "gt"),
            BinaryOp::Ge => float_cmp!(OGE, "ge"),

            _ => unreachable!("Operation {:?} not supported for floats", op),
          }
        } else {
          unreachable!("Mixed integer/float operations not yet implemented")
        }
      }

      _ => unreachable!(),
    }
  }
}
