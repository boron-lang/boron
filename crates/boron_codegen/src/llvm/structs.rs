use crate::llvm::LLVMCodegen;
use boron_ir::IrStruct;

impl LLVMCodegen<'_> {
  pub fn create_struct_type(&self, strukt: &IrStruct) {
    let struct_type = self.context.opaque_struct_type(&strukt.name);
    self.structs.insert(strukt.id, struct_type);
  }
  
  pub fn fill_struct_bodies(&self, strukt: &IrStruct) {
    let ty = self.structs.get(&strukt.id).expect("must exist");
    let mut field_types = vec![];

    for (_, ty) in &strukt.fields {
      field_types.push(self.ty(&ty));
    }

    ty.set_body(&field_types, false);
  }
}
