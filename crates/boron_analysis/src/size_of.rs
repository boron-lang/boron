use crate::align_of::{
  align_of_ty, calculate_struct_alignment, compute_discriminant_tag_size,
  substituted_enum_variant_field_tys, substituted_struct_field_tys,
};
pub(crate) use crate::{BuiltinFunctionCtx, InferTy};
use boron_resolver::{DefId, DefKind};
use boron_target::abi::layout::{Layout, align_up};

pub fn size_of_ty(ctx: &BuiltinFunctionCtx<'_>, ty: &InferTy) -> usize {
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

pub fn calculate_struct_size(
  ctx: &BuiltinFunctionCtx<'_>,
  def_id: &DefId,
  args: &Vec<InferTy>,
) -> usize {
  let field_tys = substituted_struct_field_tys(ctx, def_id, args);
  let struct_alignment = calculate_struct_alignment(ctx, def_id, args);

  let mut offset = 0;
  for field in &field_tys {
    offset = align_up(offset, align_of_ty(ctx, field).get());
    offset += size_of_ty(ctx, field);
  }

  align_up(offset, struct_alignment.get())
}

fn enum_payload_metrics(
  ctx: &BuiltinFunctionCtx<'_>,
  variant_field_tys: &[Vec<InferTy>],
) -> (usize, usize) {
  let max_payload_size = variant_field_tys
    .iter()
    .map(|fields| {
      let mut offset = 0;
      for field in fields {
        offset = align_up(offset, align_of_ty(ctx, field).get());
        offset += size_of_ty(ctx, field);
      }
      offset
    })
    .max()
    .unwrap_or(0);

  let payload_align = variant_field_tys
    .iter()
    .flat_map(|fields| fields.iter())
    .fold(1, |acc, field| acc.max(align_of_ty(ctx, field).get()));

  (max_payload_size, payload_align)
}

pub fn calculate_enum_size(
  ctx: &BuiltinFunctionCtx<'_>,
  def_id: &DefId,
  args: &Vec<InferTy>,
) -> usize {
  let e = ctx.hir.get_enum(*def_id).unwrap();
  let tag_size = compute_discriminant_tag_size(ctx, &e.variants);
  let variant_field_tys = substituted_enum_variant_field_tys(ctx, def_id, &e, args);
  let (max_payload_size, payload_align) = enum_payload_metrics(ctx, &variant_field_tys);

  if max_payload_size == 0 {
    return tag_size;
  }

  let enum_align = tag_size.max(payload_align);
  let total = align_up(tag_size, payload_align) + max_payload_size;
  align_up(total, enum_align)
}

pub fn calculate_enum_payload_layout(
  ctx: &BuiltinFunctionCtx<'_>,
  def_id: &DefId,
  args: &Vec<InferTy>,
) -> Layout {
  let e = ctx.hir.get_enum(*def_id).unwrap();
  let variant_field_tys = substituted_enum_variant_field_tys(ctx, def_id, &e, args);
  let (max_payload_size, payload_align) = enum_payload_metrics(ctx, &variant_field_tys);

  Layout::new(max_payload_size, payload_align)
}
