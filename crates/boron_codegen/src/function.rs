use crate::codegen::LLVMCodegen;
use crate::expressions::ValueKind;
use anyhow::Result;
use boron_hir::HirId;
use boron_ir::{IrFunction, IrId, Projection, SemanticTy};
use boron_resolver::DefId;
use inkwell::module::Linkage;
use inkwell::types::{
  BasicMetadataTypeEnum, BasicType as _, BasicTypeEnum, FunctionType,
};
use inkwell::values::{FunctionValue, PointerValue};

impl<'ctx> LLVMCodegen<'ctx> {
  fn params_to_metadata(
    params: &[BasicTypeEnum<'ctx>],
  ) -> Vec<BasicMetadataTypeEnum<'ctx>> {
    params.iter().map(|p| (*p).into()).collect()
  }

  pub fn fn_ret_ty(
    &self,
    ret: &SemanticTy,
    params: &[BasicTypeEnum<'ctx>],
    varargs: bool,
  ) -> Result<FunctionType<'ctx>> {
    let params = Self::params_to_metadata(params);

    match ret {
      SemanticTy::Unit => Ok(self.context.void_type().fn_type(&params, varargs)),
      _ => Ok(self.ty(ret)?.fn_type(&params, varargs)),
    }
  }

  pub fn function_value(&self, id: &IrId) -> Result<FunctionValue<'ctx>> {
    Ok(*self.require_some(self.funcs.get(id), "function value missing")?)
  }

  pub fn create_function_body(&self, func: &IrFunction) -> Result<()> {
    let params =
      func.params.iter().map(|param| self.ty(&param.ty)).collect::<Result<Vec<_>>>()?;
    let fn_ty = self.fn_ret_ty(&func.return_type, &params, false)?;

    let linkage = if func.body.is_some() { None } else { Some(Linkage::External) };
    let function = self.module.add_function(&func.name, fn_ty, linkage);

    self.funcs.insert(func.id, function);
    Ok(())
  }

  pub fn generate_function_body(&self, func: &IrFunction) -> Result<()> {
    let function = self.function_value(&func.id)?;
    self.locals.clear();

    if let Some(body) = &func.body {
      let entry = self.context.append_basic_block(function, "start");
      self.builder.position_at_end(entry);

      for (idx, param) in func.params.iter().enumerate() {
        let param_val = self
          .require_some(function.get_nth_param(idx as u32), "param index should exist")?;
        let alloca = self
          .builder
          .build_alloca(self.ty(&param.ty)?, &format!("param.alloc.{}", param.name));
        let alloca = self.require_llvm(alloca, "param alloca")?;

        let _ = self
          .require_llvm(self.builder.build_store(alloca, param_val), "param store")?;

        self.locals.insert(param.def_id, alloca);
      }

      self.generate_var_allocas(func.id)?;
      self.generate_block(&body.block)?;
      if let Some(tail) = &body.block.expr {
        self.generate_expr(tail)?;
      }

      if let Some(tail_expr) = &body.block.expr {
        match &func.return_type {
          SemanticTy::Unit => {
            let _ = self.builder.build_return(None);
          }
          _ => {
            let val = self.generate_expr(tail_expr)?;
            self.require_llvm(
              self.builder.build_return(Some(&val)),
              "couldn't build return with value",
            )?;
          }
        }
      } else {
        let _ = self.builder.build_return(None);
      }
    }

    Ok(())
  }

  pub fn generate_field(
    &self,
    struct_id: &DefId,
    args: &Vec<SemanticTy>,
    field_idx: u32,
    local_hir_id: HirId,
    struct_value: ValueKind<'ctx>,
  ) -> Result<PointerValue<'ctx>> {
    let strukt = self.ir.find_struct(struct_id, args);
    let struct_ty = self.struct_ty_by_id(&strukt.id)?;
    let ptr = match struct_value {
      ValueKind::LValue(l) => l,
      ValueKind::RValue(r) => {
        if r.is_pointer_value() {
          r.into_pointer_value()
        } else {
          let slot = self.builder.build_alloca(r.get_type(), "struct.tmp")?;
          self.builder.build_store(slot, r)?;
          slot
        }
      }
    };

    self.require_llvm(
      self.builder.build_struct_gep(
        struct_ty,
        ptr,
        field_idx,
        &format!("field.ptr.{}.{}", local_hir_id, field_idx),
      ),
      "struct field gep",
    )
  }

  pub fn generate_var_allocas(&self, function: IrId) -> Result<()> {
    let locals = self.ir.locals.get(&function);
    let Some(locals) = locals else {
      return Ok(());
    };
    for local in locals.iter() {
      let name = format!("local.alloc.{}", local.hir_id);
      let struct_ty = self.ty(&local.ty)?;
      let p_val =
        self.require_llvm(self.builder.build_alloca(struct_ty, &name), "alloca")?;
      let init_value = self.generate_expr(&local.init)?;
      let _ = self
        .require_llvm(self.builder.build_store(p_val, init_value), "store local init")?;

      for projection in &local.projections {
        match projection {
          Projection::Binding(def_id) => {
            self.locals.insert(*def_id, p_val);
          }
          Projection::Field { field_idx, struct_ty, def_id: projection_def_id } => {
            let SemanticTy::Struct { def_id: struct_def_id, args } = &struct_ty else {
              unreachable!()
            };

            let field_ptr = self.generate_field(
              struct_def_id,
              args,
              *field_idx,
              local.hir_id,
              p_val.into(),
            )?;
            if let Some(def_id) = projection_def_id {
              self.locals.insert(*def_id, field_ptr);
            }
          }
        }
      }
    }

    Ok(())
  }
}
