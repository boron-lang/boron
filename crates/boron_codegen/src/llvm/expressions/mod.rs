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
        let ptr = self.locals.get(def_id);
        if let Some(ptr) = ptr {
          self
            .builder
            .build_load(self.ty(&expr.ty), *ptr, &format!("load_{}", def_id.index()))
            .expect("load local")
        } else {
          panic!("couldn't fetch local {:#?}", def_id)
        }
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

      IrExprKind::Struct { def_id, fields, type_args } => {
        let ir_struct = self.ir.find_struct(def_id, type_args);
        let struct_ty = self.structs.get(&ir_struct.id).expect("must exist").clone();
        let alloca_name = format!("struct_init_{}", ir_struct.id.index());

        let alloca = self
          .builder
          .build_alloca(struct_ty, &alloca_name)
          .expect("couldn't create alloca");

        for (idx, (field_name, _)) in ir_struct.fields.iter().enumerate() {
          let field_ptr = self
            .builder
            .build_struct_gep(
              struct_ty,
              alloca,
              idx as u32,
              &format!("{alloca_name}_field_ptr_{idx}"),
            )
            .expect("couldn't build gep");
          let value = fields.iter().find(|field| &field.name == field_name).unwrap();

          self
            .builder
            .build_store(field_ptr, self.generate_expr(&value.value))
            .expect("couldn't build store");
        }

        self.struct_init_allocs.insert(ir_struct.id, alloca);
        let loaded =
          self.builder.build_load(struct_ty, alloca, "struct_val").expect("load struct");

        loaded
      }
      _ => todo!("{:#?}", expr),
    }
  }
}
