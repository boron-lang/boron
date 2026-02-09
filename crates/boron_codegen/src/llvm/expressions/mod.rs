use crate::llvm::LLVMCodegen;
use anyhow::Result;
use boron_ir::{IrExpr, IrExprKind};
use inkwell::values::BasicValueEnum;

mod binary_op;
mod calls;
mod literals;

impl<'ctx> LLVMCodegen<'ctx> {
  pub fn generate_expr(&self, expr: &IrExpr) -> Result<BasicValueEnum<'ctx>> {
    match &expr.kind {
      IrExprKind::Literal(literal) => self.build_literal(expr, literal),
      IrExprKind::Call { callee, type_args, args } => {
        self.generate_call(callee, type_args, args)
      }
      IrExprKind::Binary { lhs, op, rhs } => self.generate_binary_op(lhs, op, rhs),
      IrExprKind::LocalRef(def_id) => {
        let ptr = self.require_some(self.locals.get(def_id), "local pointer missing")?;
        self.require_llvm(
          self.builder.build_load(
            self.ty(&expr.ty)?,
            *ptr,
            &format!("load_{}", def_id.index()),
          ),
          "load local",
        )
      }
      IrExprKind::Array(exprs) => {
        let array_ty = self.ty(&expr.ty)?;
        let arr_id = expr.hir_id.local_id.0;

        let array_alloca =
          self.builder.build_alloca(array_ty, &format!("array_expr_{}", arr_id));
        let array_alloca = self.require_llvm(array_alloca, "array alloca")?;

        for (i, elem_expr) in exprs.iter().enumerate() {
          let elem_value = self.generate_expr(elem_expr)?;

          let elem_ptr = unsafe {
            self.require_llvm(
              self.builder.build_gep(
                array_ty,
                array_alloca,
                &[
                  self.context.i32_type().const_int(0, false),
                  self.context.i32_type().const_int(i as u64, false),
                ],
                &format!("array_elem_ptr_{}_{}", arr_id, i),
              ),
              "array element gep",
            )?
          };

          let _ = self.require_llvm(
            self.builder.build_store(elem_ptr, elem_value),
            "array element store",
          )?;
        }

        Ok(array_alloca.into())
      }

      IrExprKind::Struct { def_id, fields, type_args } => {
        let ir_struct = self.ir.find_struct(def_id, type_args);
        let struct_ty = self.struct_ty_by_id(&ir_struct.id)?;
        let alloca_name = format!("struct_init_{}", ir_struct.id.index());

        let alloca = self.builder.build_alloca(struct_ty, &alloca_name);
        let alloca = self.require_llvm(alloca, "struct alloca")?;

        for (idx, (field_name, _)) in ir_struct.fields.iter().enumerate() {
          let field_ptr = self.require_llvm(
            self.builder.build_struct_gep(
              struct_ty,
              alloca,
              idx as u32,
              &format!("{alloca_name}_field_ptr_{idx}"),
            ),
            "struct field gep",
          )?;
          let value = self.require_some(
            fields.iter().find(|field| &field.name == field_name),
            "struct field missing in initializer",
          )?;

          let _ = self.require_llvm(
            self.builder.build_store(field_ptr, self.generate_expr(&value.value)?),
            "struct field store",
          )?;
        }

        self.struct_init_allocs.insert(ir_struct.id, alloca);
        let loaded = self.require_llvm(
          self.builder.build_load(struct_ty, alloca, "struct_val"),
          "load struct",
        )?;

        Ok(loaded)
      }
      _ => todo!("{:#?}", expr),
    }
  }
}
