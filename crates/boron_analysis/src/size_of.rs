use crate::align_of::{
  align_of_ty, calculate_struct_alignment, substituted_struct_field_tys,
};
pub(crate) use crate::{BuiltinFunctionCtx, InferTy};
use boron_resolver::{DefId, DefKind};
use boron_target::abi::layout::align_up;

pub fn size_of_ty<'a>(ctx: &'a BuiltinFunctionCtx<'a>, ty: &InferTy) -> usize {
  let target = ctx.sess.target();

  match ty {
    InferTy::Primitive(p, _) => target.size_of(*p),
    InferTy::Ptr { .. } => target.pointer_width.size_bytes(),
    InferTy::Array { ty, len, .. } => size_of_ty(ctx, ty) * len.len(),
    InferTy::Adt { def_id, args, .. } => {
      let def = ctx.resolver.get_definition(*def_id).unwrap();

      match def.kind {
        DefKind::Struct => calculate_struct_size(ctx, def_id, args),
        DefKind::Enum => calculate_enum_size(ctx, def_id, args),
        _ => unreachable!(),
      }
    }

    _ => todo!("size_of_ty not implemented for {ty:?}"),
  }
}

pub fn calculate_struct_size<'a>(
  ctx: &'a BuiltinFunctionCtx<'a>,
  def_id: &DefId,
  args: &Vec<InferTy>,
) -> usize {
  let field_tys = substituted_struct_field_tys(ctx, def_id, args);
  let struct_alignment = calculate_struct_alignment(ctx, def_id, args);

  let mut offset = 0;
  for substituted_field in &field_tys {
    let (size_f, align_f) =
      (size_of_ty(ctx, substituted_field), align_of_ty(ctx, substituted_field));

    offset = align_up(offset, align_f.get());
    offset += size_f;
  }

  offset = align_up(offset, struct_alignment.get());
  offset
}

pub fn calculate_enum_size<'a>(
  sz: &'a BuiltinFunctionCtx<'a>,
  def_id: &DefId,
  args: &Vec<InferTy>,
) -> usize {
  let e = sz.hir.get_enum(*def_id).unwrap();
  todo!()
}
