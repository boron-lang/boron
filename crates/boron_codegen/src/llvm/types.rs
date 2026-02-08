use crate::llvm::LLVMCodegen;
use boron_ir::SemanticTy;
use boron_parser::PrimitiveKind;
use boron_utils::prelude::warn;
use inkwell::AddressSpace;
use inkwell::types::{BasicType, BasicTypeEnum};

impl<'ctx> LLVMCodegen<'ctx> {
  pub fn primitive_ty(&self, prim: &PrimitiveKind) -> BasicTypeEnum<'ctx> {
    let ctx = self.context;
    match prim {
      PrimitiveKind::I8 => ctx.i8_type().into(),
      PrimitiveKind::I16 => ctx.i16_type().into(),
      PrimitiveKind::I32 => ctx.i32_type().into(),
      PrimitiveKind::I64 => ctx.i64_type().into(),
      PrimitiveKind::I128 => ctx.i128_type().into(),
      PrimitiveKind::ISize => {
        let bits = self.ctx.target().pointer_width.size_bytes() * 8;
        ctx.custom_width_int_type(bits as u32).into()
      }

      PrimitiveKind::U8 => ctx.i8_type().into(),
      PrimitiveKind::U16 => ctx.i16_type().into(),
      PrimitiveKind::U32 => ctx.i32_type().into(),
      PrimitiveKind::U64 => ctx.i64_type().into(),
      PrimitiveKind::U128 => ctx.i128_type().into(),
      PrimitiveKind::USize => {
        let bits = self.ctx.target().pointer_width.size_bytes() * 8;
        ctx.custom_width_int_type(bits as u32).into()
      }

      PrimitiveKind::F32 => ctx.f32_type().into(),
      PrimitiveKind::F64 => ctx.f64_type().into(),

      PrimitiveKind::Bool => ctx.bool_type().into(),
      PrimitiveKind::Char => ctx.i32_type().into(),
    }
  }

  pub fn ty(&self, ty: &SemanticTy) -> BasicTypeEnum<'ctx> {
    match ty {
      SemanticTy::Primitive(p) => self.primitive_ty(p),

      SemanticTy::Struct { def_id, args } => {
        let strukt = self.ir.find_struct(def_id, args);
        self.structs.get(&strukt.id).expect("must exist").value().as_basic_type_enum()
      }

      SemanticTy::Ptr { .. } => {
        self.context.ptr_type(AddressSpace::default()).as_basic_type_enum()
      }
      SemanticTy::Unit => panic!("unit has no value"),
      SemanticTy::Array { elem, len }=> {
        let elem_ty = self.ty(elem);
        elem_ty.array_type(*len as u32).as_basic_type_enum()
      }

      _ => {
        warn!("not handled: {:#?}", ty);
        self.context.i8_type().into()
      }
    }
  }
}
