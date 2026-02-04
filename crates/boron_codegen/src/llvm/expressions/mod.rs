use crate::llvm::LLVMCodegen;
use boron_ir::{IrExpr, IrExprKind};
use inkwell::values::BasicValueEnum;

mod binary_op;
mod calls;
mod literals;

impl<'ctx> LLVMCodegen<'ctx> {
  pub fn generate_expr(&self, expr: &IrExpr) -> BasicValueEnum<'ctx> {
    match &expr.kind {
      IrExprKind::Literal(literal) => self.build_literal(expr, literal),
      IrExprKind::Call { callee, type_args, args } => {
        self.generate_call(callee, type_args, args)
      }
      IrExprKind::Binary { lhs, op, rhs } => self.generate_binary_op(lhs, op, rhs),
      IrExprKind::LocalRef(def_id) => {
        let ptr = self.locals.get(def_id).expect("local ref should be in locals map");
        self
          .builder
          .build_load(self.ty(&expr.ty), *ptr, &format!("load_{}", def_id.index()))
          .expect("load local")
      }
      _ => todo!("{:#?}", expr),
    }
  }
}
