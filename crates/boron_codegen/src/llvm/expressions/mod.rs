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
      IrExprKind::Array(exprs) => {
        let array_ty = self.ty(&expr.ty);
        let arr_id = expr.hir_id.local_id.0;

        let array_alloca = self
          .builder
          .build_alloca(array_ty, &format!("array_expr_{}", arr_id))
          .expect("alloca failed");

        for (i, elem_expr) in exprs.iter().enumerate() {
          let elem_value = self.generate_expr(elem_expr);

          let elem_ptr = unsafe {
            self
              .builder
              .build_gep(
                array_ty,
                array_alloca,
                &[
                  self.context.i32_type().const_int(0, false),
                  self.context.i32_type().const_int(i as u64, false),
                ],
                &format!("array_elem_ptr_{}_{}", arr_id, i),
              )
              .expect("gep failed")
          };

          self.builder.build_store(elem_ptr, elem_value).expect("store failed");
        }

        array_alloca.into()
      }
      _ => todo!("{:#?}", expr),
    }
  }
}
