use crate::llvm::LLVMCodegen;
use anyhow::Result;
use boron_ir::IrStruct;

impl LLVMCodegen<'_> {
  pub fn create_struct_type(&self, strukt: &IrStruct) {
    let struct_type = self.context.opaque_struct_type(&strukt.name);
    self.structs.insert(strukt.id, struct_type);
  }

  pub fn generate_struct_body(&self, strukt: &IrStruct) -> Result<()> {
    let ty =
      self.require_some(self.structs.get(&strukt.id), "struct type missing for body")?;
    let mut field_types = vec![];

    for (_, ty) in &strukt.fields {
      field_types.push(self.ty(ty)?);
    }

    ty.set_body(&field_types, false);
    Ok(())
  }
}
