use crate::{BuiltinFunctionCtx, InferTy, TyChecker};
use boron_resolver::{DefId, DefKind};
use boron_target::abi::layout::Alignment;

pub fn align_of_ty<'a>(sz: &'a BuiltinFunctionCtx<'a>, ty: &InferTy) -> Alignment {
  let target = sz.sess.target();

  match ty {
    InferTy::Primitive(p, _) => target.size_of(*p).into(),
    InferTy::Ptr { .. } => target.pointer_width.size_bytes().into(),
    InferTy::Array { ty, len, .. } => align_of_ty(sz, ty),
    InferTy::Adt { def_id, args, .. } => {
      let def = sz.resolver.get_definition(*def_id).unwrap();
      match def.kind {
        DefKind::Struct => calculate_struct_alignment(sz, def_id, args),
        DefKind::Enum => calculate_enum_alignment(sz, def_id, args),
        _ => unreachable!(),
      }
    }

    _ => Alignment::default(),
  }
}

pub fn calculate_struct_alignment<'a>(
  sz: &'a BuiltinFunctionCtx<'a>,
  def_id: &DefId,
  args: &Vec<InferTy>,
) -> Alignment {
  let strct = sz.hir.get_struct(*def_id).unwrap();
  let fields = &strct.fields;

  fields
    .iter()
    .fold(0, |acc, field| {
      let field_ty = sz.ty_table.field_type(*def_id, field.name).unwrap();
      acc.max(align_of_ty(sz, &field_ty).get())
    })
    .into()
}

pub fn calculate_enum_alignment<'a>(
  sz: &'a BuiltinFunctionCtx<'a>,
  def_id: &DefId,
  args: &Vec<InferTy>,
) -> Alignment {
  let e = sz.hir.get_enum(*def_id).unwrap();
  todo!()
}
