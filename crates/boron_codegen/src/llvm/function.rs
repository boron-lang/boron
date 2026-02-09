use crate::llvm::blocks::{BlockContext, BlockGeneratorContext};
use crate::llvm::LLVMCodegen;
use boron_ir::{IrFunction, IrId, Projection, SemanticTy};
use inkwell::module::Linkage;
use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType};
use inkwell::values::FunctionValue;
use itertools::Itertools;

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
  ) -> FunctionType<'ctx> {
    let params = Self::params_to_metadata(params);

    match ret {
      SemanticTy::Unit => self.context.void_type().fn_type(&params, varargs),
      _ => self.ty(ret).fn_type(&params, varargs),
    }
  }

  pub fn function_value(&self, id: &IrId) -> FunctionValue<'ctx> {
    self.funcs.get(id).expect("should exist").clone()
  }

  pub fn create_function_body(&self, func: &IrFunction) {
    let params = func.params.iter().map(|param| self.ty(&param.ty)).collect_vec();
    let fn_ty = self.fn_ret_ty(&func.return_type, &params, false);

    let linkage = if func.body.is_some() { None } else { Some(Linkage::External) };
    let function = self.module.add_function(&func.name, fn_ty, linkage);

    self.funcs.insert(func.id, function);
  }

  pub fn generate_function_body(&self, func: &IrFunction) {
    let function = self.function_value(&func.id);
    self.locals.clear();

    if let Some(body) = &func.body {
      for block in &body.blocks {
        let context = if block.hir_id == body.entry {
          BlockContext::FunctionStart
        } else {
          BlockContext::Normal
        };

        self.generate_block(&BlockGeneratorContext::new(block, func, function, context));
      }
    }
  }

  pub fn generate_var_allocas(&self, function: IrId) {
    for local in self.ir.locals.get(&function).expect("should exist").iter() {
      let name = format!("local_{}", local.hir_id.index());
      let struct_ty = self.ty(&local.ty);
      let p_val = self.builder.build_alloca(struct_ty, &name).expect("alloca");
      self
        .builder
        .build_store(p_val, self.generate_expr(&local.init))
        .expect("failed to build store instruction");

      for projection in &local.projections {
        match projection {
          Projection::Binding(def_id) => {
            self.locals.insert(*def_id, p_val);
          }
          Projection::Field { field_idx, struct_ty, def_id: projection_def_id } => {
            let SemanticTy::Struct { def_id: struct_def_id, args } = &struct_ty else {
              unreachable!()
            };

            let strukt = self.ir.find_struct(&struct_def_id, &args);
            let struct_ty = self.structs.get(&strukt.id).unwrap();
            let field_ty = struct_ty
              .get_field_type_at_index(*field_idx)
              .expect("couldn't extract type");

            let struct_ptr =
              self.struct_init_allocs.get(&strukt.id).expect("should exist");
            let field_ptr = self
              .builder
              .build_struct_gep(
                *struct_ty,
                *struct_ptr,
                *field_idx,
                &format!("field_access_{}", local.hir_id.index()),
              )
              .expect("invalid field index");

            if let Some(def_id) = projection_def_id {
              self.locals.insert(*def_id, field_ptr);
            }
          }
        }
      }
    }
  }
}
