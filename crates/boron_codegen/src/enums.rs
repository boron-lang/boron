use crate::codegen::LLVMCodegen;
use anyhow::Result;
use boron_ir::IrEnum;

impl LLVMCodegen<'_> {
  pub fn create_enum_type(&self, enum_: &IrEnum) {
    let struct_type = self.context.opaque_struct_type(&enum_.name);
    self.structs.insert(enum_.id, struct_type);
  }

  pub fn generate_enum_bodies(&self, enum_: &IrEnum) -> Result<()> {
    let ty =
      self.require_some(self.structs.get(&enum_.id), "enum type missing for body")?;

    ty.set_body(&[self.context.i64_type().into()], false);
    Ok(())
  }
}
