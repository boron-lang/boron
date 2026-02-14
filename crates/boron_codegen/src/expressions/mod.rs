use crate::codegen::LLVMCodegen;
use anyhow::Result;
use boron_ir::{IrExpr, IrExprKind, SemanticTy};
use inkwell::llvm_sys::prelude::LLVMValueRef;
use inkwell::types::BasicTypeEnum;
use inkwell::values::{AnyValue, AsValueRef, BasicValue, BasicValueEnum, PointerValue};
use std::fmt::Debug;

mod binary_op;
pub mod blocks;
mod calls;
mod literals;

#[derive(Debug)]
pub enum ValueKind<'ctx> {
  LValue(PointerValue<'ctx>),
  RValue(BasicValueEnum<'ctx>),
}

unsafe impl<'ctx> AnyValue<'ctx> for ValueKind<'ctx> {}

unsafe impl<'ctx> AsValueRef for ValueKind<'ctx> {
  fn as_value_ref(&self) -> LLVMValueRef {
    match self {
      ValueKind::LValue(l) => l.as_value_ref(),
      ValueKind::RValue(r) => r.as_value_ref(),
    }
  }
}

unsafe impl<'ctx> BasicValue<'ctx> for ValueKind<'ctx> {}

impl<'ctx> From<BasicValueEnum<'ctx>> for ValueKind<'ctx> {
  fn from(value: BasicValueEnum<'ctx>) -> Self {
    Self::RValue(value)
  }
}

impl<'ctx> From<PointerValue<'ctx>> for ValueKind<'ctx> {
  fn from(value: PointerValue<'ctx>) -> Self {
    Self::LValue(value)
  }
}

impl<'ctx> LLVMCodegen<'ctx> {
  pub fn value_to_basic(
    &self,
    ty: BasicTypeEnum<'ctx>,
    value: ValueKind<'ctx>,
  ) -> Result<BasicValueEnum<'ctx>> {
    match value {
      ValueKind::RValue(val) => Ok(val),
      ValueKind::LValue(ptr) => Ok(self.builder.build_load(ty, ptr, "lval.load.tmp")?),
    }
  }

  pub fn generate_expr(&self, expr: &IrExpr) -> Result<ValueKind<'ctx>> {
    match &expr.kind {
      IrExprKind::Literal(literal) => {
        Ok(ValueKind::RValue(self.build_literal(expr, literal)?))
      }
      IrExprKind::Call { callee, type_args, args } => {
        Ok(ValueKind::RValue(self.generate_call(callee, type_args, args)?))
      }
      IrExprKind::Binary { lhs, op, rhs } => {
        Ok(ValueKind::RValue(self.generate_binary_op(lhs, op, rhs)?))
      }
      IrExprKind::LocalRef(def_id) => {
        let ptr = self.require_some(self.locals.get(def_id), "local pointer missing")?;
        Ok(
          self
            .require_llvm(
              self.builder.build_load(
                self.ty(&expr.ty)?,
                *ptr,
                &format!("local.load.{}", expr.hir_id),
              ),
              "load local",
            )?
            .into(),
        )
      }
      IrExprKind::Array(exprs) => {
        let array_ty = self.ty(&expr.ty)?;
        let array_alloca =
          self.builder.build_alloca(array_ty, &format!("array.alloc.{}", expr.hir_id));
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
                &format!("array.elem.ptr.{}.{}", expr.hir_id, i),
              ),
              "array element gep",
            )?
          };

          let value = self.value_to_basic(self.ty(&elem_expr.ty)?, elem_value)?; 
          let _ = self.require_llvm(
            self.builder.build_store(elem_ptr, value),
            "array element store",
          )?;
        }

        Ok(array_alloca.into())
      }

      IrExprKind::Struct { def_id, fields, type_args } => {
        let ir_struct = self.ir.find_struct(def_id, type_args);
        let struct_ty = self.struct_ty_by_id(&ir_struct.id)?;
        let alloca_name = format!("struct.init.alloc.{}", expr.hir_id);

        let alloca = self.builder.build_alloca(struct_ty, &alloca_name);
        let alloca = self.require_llvm(alloca, "struct alloca")?;

        for (idx, (field_name, _)) in ir_struct.fields.iter().enumerate() {
          let field_ptr = self.require_llvm(
            self.builder.build_struct_gep(
              struct_ty,
              alloca,
              idx as u32,
              &format!("struct.field.ptr.{}.{}", expr.hir_id, idx),
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
        // let loaded = self.require_llvm(
        //   self.builder.build_load(
        //     struct_ty,
        //     alloca,
        //     &format!("struct.load.{}", expr.hir_id),
        //   ),
        //   "load struct",
        // )?;

        Ok(alloca.into())
      }
      IrExprKind::Block(block) => {
        self.generate_block(block)?;
        if let Some(tail) = &block.expr { self.generate_expr(tail) } else { panic!() }
      }
      IrExprKind::If { condition, then_block, else_branch } => {
        let function = self.builder.get_insert_block().unwrap().get_parent().unwrap();
        let cond_val = self.value_to_basic(self.ty(&condition.ty)?, self.generate_expr(condition)?);

        let then_bb = self
          .context
          .append_basic_block(function, &format!("then.{}", then_block.hir_id));
        let else_bb = self.context.append_basic_block(
          function,
          &format!(
            "else.{}",
            else_branch
              .as_ref()
              .map(|b| b.hir_id.to_string())
              .unwrap_or("none".to_string())
          ),
        );
        let merge_bb = self.context.append_basic_block(function, "ifcont");

        self.require_llvm(
          self.builder.build_conditional_branch(
            cond_val?.into_int_value(),
            then_bb,
            else_bb,
          ),
          "if conditional branch",
        )?;

        let produces_value = !matches!(expr.ty, SemanticTy::Unit | SemanticTy::Never);

        self.builder.position_at_end(then_bb);
        self.generate_block(then_block)?;
        let then_val = if let Some(tail) = &then_block.expr {
          Some(self.generate_expr(tail)?)
        } else {
          None
        };
        let then_end_bb = self.builder.get_insert_block().unwrap();
        self.require_llvm(
          self.builder.build_unconditional_branch(merge_bb),
          "then -> merge branch",
        )?;

        self.builder.position_at_end(else_bb);
        let else_val = if let Some(else_expr) = else_branch {
          Some(self.generate_expr(else_expr)?)
        } else {
          None
        };
        let else_end_bb = self.builder.get_insert_block().unwrap();
        self.require_llvm(
          self.builder.build_unconditional_branch(merge_bb),
          "else -> merge branch",
        )?;

        self.builder.position_at_end(merge_bb);

        if produces_value && let (Some(then_v), Some(else_v)) = (then_val, else_val) {
          let phi = self.require_llvm(
            self.builder.build_phi(self.ty(&expr.ty)?, "if.val"),
            "if phi node",
          )?;
          phi.add_incoming(&[(&then_v, then_end_bb), (&else_v, else_end_bb)]);
          Ok(phi.as_basic_value().into())
        } else {
          Ok(self.context.bool_type().const_int(0, false).as_basic_value_enum().into())
        }
      }
      IrExprKind::Loop { body } => {
        let function = self.builder.get_insert_block().unwrap().get_parent().unwrap();
        let loop_bb = self.context.append_basic_block(function, "loop");
        let exit_bb = self.context.append_basic_block(function, "loopexit");

        self.loop_exit_blocks.borrow_mut().push(exit_bb);
        self.loop_header_blocks.borrow_mut().push(loop_bb);

        self
          .require_llvm(self.builder.build_unconditional_branch(loop_bb), "enter loop")?;

        self.builder.position_at_end(loop_bb);
        self.generate_block(body)?;
        if let Some(tail) = &body.expr {
          let _ = self.generate_expr(tail)?;
        }
        self.require_llvm(
          self.builder.build_unconditional_branch(loop_bb),
          "loop back-edge",
        )?;

        self.loop_header_blocks.borrow_mut().pop();
        self.loop_exit_blocks.borrow_mut().pop();

        self.builder.position_at_end(exit_bb);
        Ok(self.context.bool_type().const_int(0, false).as_basic_value_enum().into())
      }
      IrExprKind::Break => {
        let exit_bb =
          *self.loop_exit_blocks.borrow().last().expect("break outside of loop");
        self.require_llvm(self.builder.build_unconditional_branch(exit_bb), "break")?;
        let function = self.builder.get_insert_block().unwrap().get_parent().unwrap();
        let dead_bb = self.context.append_basic_block(function, "after.break");
        self.builder.position_at_end(dead_bb);
        Ok(self.context.bool_type().const_int(0, false).as_basic_value_enum().into())
      }
      IrExprKind::Continue => {
        let header_bb =
          *self.loop_header_blocks.borrow().last().expect("continue outside of loop");
        self
          .require_llvm(self.builder.build_unconditional_branch(header_bb), "continue")?;
        let function = self.builder.get_insert_block().unwrap().get_parent().unwrap();
        let dead_bb = self.context.append_basic_block(function, "after.continue");
        self.builder.position_at_end(dead_bb);
        Ok(self.context.bool_type().const_int(0, false).as_basic_value_enum().into())
      }
      IrExprKind::Return { value } => {
        if let Some(val) = value {
          let ret_val = self.generate_expr(val)?;
          self.require_llvm(
            self.builder.build_return(Some(&ret_val)),
            "return with value",
          )?;
        } else {
          let _ = self.builder.build_return(None);
        }
        let function = self.builder.get_insert_block().unwrap().get_parent().unwrap();
        let dead_bb = self.context.append_basic_block(function, "after.return");
        self.builder.position_at_end(dead_bb);
        Ok(self.context.bool_type().const_int(0, false).as_basic_value_enum().into())
      }
      IrExprKind::Field { object, field } => {
        let SemanticTy::Struct { def_id, args } = &object.ty else { unreachable!() };
        let strukt = self.ir.find_struct(def_id, args);
        let field_idx = strukt
          .fields
          .iter()
          .position(|(name, _)| name == &field.text())
          .expect("couldn't find field index");

        let field_ptr =
          self.generate_field(def_id, args, field_idx as u32, expr.hir_id)?;
        Ok(field_ptr.into())
      }
      IrExprKind::Skip => Err(anyhow::anyhow!("skip expression has no value")),
      _ => todo!("{:#?}", expr),
    }
  }
}
