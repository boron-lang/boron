use crate::primitive::PrimitiveKind;
use crate::target::Endian;
use inkwell::AddressSpace;
use inkwell::context::Context;
use inkwell::targets::{ByteOrdering, TargetData};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct DataLayout {
  pub pointer_size: usize,
  pub pointer_align: usize,

  pub i8_size: usize,
  pub i8_align: usize,

  pub i16_size: usize,
  pub i16_align: usize,

  pub i32_size: usize,
  pub i32_align: usize,

  pub i64_size: usize,
  pub i64_align: usize,

  pub i128_size: usize,
  pub i128_align: usize,

  pub f32_size: usize,
  pub f32_align: usize,

  pub f64_size: usize,
  pub f64_align: usize,

  pub bool_size: usize,
  pub bool_align: usize,

  pub char_size: usize,
  pub char_align: usize,

  pub void_size: usize,
  pub void_align: usize,

  pub endian: Endian,
}

impl DataLayout {
  pub fn size_of(&self, primitive: PrimitiveKind) -> usize {
    match primitive {
      PrimitiveKind::I8 | PrimitiveKind::U8 => self.i8_size,
      PrimitiveKind::I16 | PrimitiveKind::U16 => self.i16_size,
      PrimitiveKind::I32 | PrimitiveKind::U32 => self.i32_size,
      PrimitiveKind::I64 | PrimitiveKind::U64 => self.i64_size,
      PrimitiveKind::I128 | PrimitiveKind::U128 => self.i128_size,
      PrimitiveKind::ISize | PrimitiveKind::USize => self.pointer_size,
      PrimitiveKind::F32 => self.f32_size,
      PrimitiveKind::F64 => self.f64_size,
      PrimitiveKind::Bool => self.bool_size,
      PrimitiveKind::Char => self.char_size,
      PrimitiveKind::Void => self.void_size,
    }
  }

  pub fn align_of(&self, primitive: PrimitiveKind) -> usize {
    match primitive {
      PrimitiveKind::I8 | PrimitiveKind::U8 => self.i8_align,
      PrimitiveKind::I16 | PrimitiveKind::U16 => self.i16_align,
      PrimitiveKind::I32 | PrimitiveKind::U32 => self.i32_align,
      PrimitiveKind::I64 | PrimitiveKind::U64 => self.i64_align,
      PrimitiveKind::I128 | PrimitiveKind::U128 => self.i128_align,
      PrimitiveKind::ISize | PrimitiveKind::USize => self.pointer_align,
      PrimitiveKind::F32 => self.f32_align,
      PrimitiveKind::F64 => self.f64_align,
      PrimitiveKind::Bool => self.bool_align,
      PrimitiveKind::Char => self.char_align,
      PrimitiveKind::Void => self.void_align,
    }
  }

  pub fn create_from_llvm(target: &TargetData) -> Self {
    let context = Context::create();

    let i1 = context.bool_type();
    let i8 = context.i8_type();
    let i16 = context.i16_type();
    let i32 = context.i32_type();
    let i64 = context.i64_type();
    let i128 = context.i128_type();

    let f32 = context.f32_type();
    let f64 = context.f64_type();

    let ptr = context.ptr_type(AddressSpace::default());

    let endian = match target.get_byte_ordering() {
      ByteOrdering::LittleEndian => Endian::Little,
      ByteOrdering::BigEndian => Endian::Big,
    };

    Self {
      pointer_size: target.get_pointer_byte_size(None) as usize,
      pointer_align: target.get_abi_alignment(&ptr) as usize,

      i8_size: target.get_abi_size(&i8) as usize,
      i8_align: target.get_abi_alignment(&i8) as usize,

      i16_size: target.get_abi_size(&i16) as usize,
      i16_align: target.get_abi_alignment(&i16) as usize,

      i32_size: target.get_abi_size(&i32) as usize,
      i32_align: target.get_abi_alignment(&i32) as usize,

      i64_size: target.get_abi_size(&i64) as usize,
      i64_align: target.get_abi_alignment(&i64) as usize,

      i128_size: target.get_abi_size(&i128) as usize,
      i128_align: target.get_abi_alignment(&i128) as usize,

      f32_size: target.get_abi_size(&f32) as usize,
      f32_align: target.get_abi_alignment(&f32) as usize,

      f64_size: target.get_abi_size(&f64) as usize,
      f64_align: target.get_abi_alignment(&f64) as usize,

      bool_size: target.get_abi_size(&i1) as usize,
      bool_align: target.get_abi_alignment(&i1) as usize,

      char_size: target.get_abi_size(&i32) as usize,
      char_align: target.get_abi_alignment(&i32) as usize,

      void_size: target.get_abi_size(&i8) as usize,
      void_align: target.get_abi_alignment(&i8) as usize,

      endian,
    }
  }
}
