use crate::codegen::LLVMCodegen;
use crate::expressions::ValueKind;
use anyhow::Result;
use boron_ir::{IrEnum, IrExpr};
use boron_session::prelude::Identifier;
use inkwell::types::BasicTypeEnum;
use inkwell::AddressSpace;

impl<'a> LLVMCodegen<'a> {
  pub fn create_enum_type(&self, enum_: &IrEnum) {
    let struct_type = self.context.opaque_struct_type(&enum_.name);
    self.structs.insert(enum_.id, struct_type);
  }

  pub fn generate_enum_bodies(&self, enum_: &IrEnum) -> Result<()> {
    let ty =
      self.require_some(self.structs.get(&enum_.id), "enum type missing for body")?;

    let tag_type = self.discriminant_ty(enum_);

    let payload = self.context.i8_type().array_type(enum_.payload_layout.size as u32);
    ty.set_body(&[tag_type, payload.into()], false);
    Ok(())
  }

  pub fn discriminant_ty(&self, enum_: &IrEnum) -> BasicTypeEnum<'a> {
    let largest = enum_.variants.iter().map(|d| d.discriminant).max();

    largest
      .map(|disc| self.type_for_discriminant(disc))
      .unwrap_or(self.context.i8_type().into())
  }

  pub fn type_for_discriminant(&self, discriminant: u128) -> BasicTypeEnum<'a> {
    if discriminant <= u8::MAX as u128 {
      self.context.i8_type()
    } else if discriminant <= u16::MAX as u128 {
      self.context.i16_type()
    } else if discriminant <= u32::MAX as u128 {
      self.context.i32_type()
    } else if discriminant <= u64::MAX as u128 {
      self.context.i64_type()
    } else {
      self.context.i128_type()
    }
    .into()
  }

  pub fn build_enum_tuple(
    &self,
    enum_: &IrEnum,
    variant: &Identifier,
    args: &Vec<IrExpr>,
  ) -> Result<ValueKind<'a>> {
    let variant =
      enum_.variants.iter().find(|v| v.name == variant.text()).expect("no enum found");

    let enum_ty = self.struct_ty_by_id(&enum_.id)?;
    let alloca =
      self.builder.build_alloca(enum_ty, &format!("enum.alloca.{}", enum_.id))?;

    let disc_ty = self.discriminant_ty(enum_);
    let tag_ptr = self.builder.build_struct_gep(enum_ty, alloca, 0, "tag")?;
    self.builder.build_store(
      tag_ptr,
      disc_ty.into_int_type().const_int(variant.discriminant as u64, false),
    )?;

    if let Some(payload_ty) = &variant.payload {
      let payload_ptr = self.builder.build_struct_gep(enum_ty, alloca, 1, "payload")?;
      let payload_cast = self.builder.build_pointer_cast(
        payload_ptr,
        self.context.ptr_type(AddressSpace::default()),
        "payload_cast",
      )?;
      let tuple = self.build_tuple(args)?;
      self.builder.build_store(payload_cast, tuple)?;

      Ok(ValueKind::LValue(alloca))
    } else {
      todo!("enums with no payload")
    }
  }
}
