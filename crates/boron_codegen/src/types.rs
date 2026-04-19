use crate::codegen::LLVMCodegen;
use anyhow::{anyhow, Result};
use boron_ir::SemanticTy;
use boron_parser::PrimitiveKind;
use boron_resolver::DefId;
use boron_session::prelude::warn;
use inkwell::types::{BasicType as _, BasicTypeEnum, StructType};
use inkwell::AddressSpace;
use std::num::NonZeroU32;

impl<'ctx> LLVMCodegen<'ctx> {
  pub fn primitive_ty(&self, prim: &PrimitiveKind) -> Result<BasicTypeEnum<'ctx>> {
    let ctx = self.context;
    Ok(match prim {
      PrimitiveKind::I8 | PrimitiveKind::U8 => ctx.i8_type().into(),
      PrimitiveKind::I16 | PrimitiveKind::U16 => ctx.i16_type().into(),
      PrimitiveKind::I32 | PrimitiveKind::U32 | PrimitiveKind::Char => {
        ctx.i32_type().into()
      }
      PrimitiveKind::I64 | PrimitiveKind::U64 => ctx.i64_type().into(),
      PrimitiveKind::I128 | PrimitiveKind::U128 => ctx.i128_type().into(),
      PrimitiveKind::ISize | PrimitiveKind::USize => {
        let bits = self.sess.target().pointer_width.size_bytes() * 8;
        ctx
          .custom_width_int_type(NonZeroU32::new(bits as u32).unwrap())
          .map_err(|err| anyhow!("Failed to construct usize/isize ty {err}"))?
          .into()
      }

      PrimitiveKind::F32 => ctx.f32_type().into(),
      PrimitiveKind::F64 => ctx.f64_type().into(),

      PrimitiveKind::Bool => ctx.bool_type().into(),
      PrimitiveKind::Void => ctx.i8_type().into(),
    })
  }

  pub fn get_struct_ty(
    &self,
    def_id: &DefId,
    args: &Vec<SemanticTy>,
  ) -> Result<StructType<'ctx>> {
    let strukt = self.ir.find_struct(def_id, args);
    self.struct_ty_by_id(&strukt.id)
  }

  pub fn get_enum_ty(
    &self,
    def_id: &DefId,
    args: &Vec<SemanticTy>,
  ) -> Result<StructType<'ctx>> {
    let strukt = self.ir.find_enum(def_id, args);
    self.struct_ty_by_id(&strukt.id)
  }

  pub fn ty(&self, ty: &SemanticTy) -> Result<BasicTypeEnum<'ctx>> {
    match ty {
      SemanticTy::Primitive(p) => self.primitive_ty(p),

      SemanticTy::Struct { def_id, args } => {
        Ok(self.get_struct_ty(def_id, args)?.as_basic_type_enum())
      }

      SemanticTy::Enum { def_id, args, .. } => {
        Ok(self.get_enum_ty(def_id, args)?.as_basic_type_enum())
      }

      SemanticTy::Ptr { .. } => {
        Ok(self.context.ptr_type(AddressSpace::default()).as_basic_type_enum())
      }
      SemanticTy::Unit => Err(anyhow!("unit has no value")),
      SemanticTy::Array { elem, len } => {
        let elem_ty = self.ty(elem)?;
        Ok(elem_ty.array_type(*len as u32).as_basic_type_enum())
      }

      SemanticTy::Tuple(elements) => self.tuple_ty(elements),

      _ => {
        warn!("not handled: {ty:#?}");
        Ok(self.context.i8_type().into())
      }
    }
  }

  pub fn tuple_ty(&self, elements: &Vec<SemanticTy>) -> Result<BasicTypeEnum<'ctx>> {
    let mut fields = vec![];
    for element in elements {
      fields.push(self.ty(element)?.as_basic_type_enum());
    }

    let ty = self.context.struct_type(fields.as_slice(), false);

    Ok(ty.into())
  }
}
