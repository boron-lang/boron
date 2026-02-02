use crate::InferTy;
use boron_hir::Hir;
use boron_resolver::{DefId, DefKind, Resolver};
use boron_utils::context::Context;

pub struct SizeOfContext<'a> {
  pub ctx: &'a Context<'a>,
  pub resolver: &'a Resolver,
  pub hir: &'a Hir,
}

pub fn size_of_ty<'a>(sz: &'a SizeOfContext<'a>, ty: &InferTy) -> usize {
  let target = sz.ctx.session.target();

  match ty {
    InferTy::Primitive(p, _) => target.size_of(*p),
    InferTy::Ptr { .. } => target.pointer_width.size_bytes(),
    InferTy::Array { ty, len, .. } => size_of_ty(sz, ty) * len,
    InferTy::Adt { def_id, .. } => {
      let def = sz.resolver.get_definition(*def_id).unwrap();

      match def.kind {
        DefKind::Struct => calculate_struct_size(sz, def_id),
        DefKind::Enum => calculate_enum_size(sz, def_id),
        _ => unreachable!(),
      }
    }

    _ => 0,
  }
}

pub fn calculate_struct_size<'a>(sz: &'a SizeOfContext<'a>, def_id: &DefId) -> usize {
  let strct = sz.hir.get_struct(*def_id).unwrap();
  let fields = &strct.fields;

  0
}

pub fn calculate_enum_size<'a>(sz: &'a SizeOfContext<'a>, def_id: &DefId) -> usize {
  let e = sz.hir.get_enum(*def_id).unwrap();
  todo!()
}
