use crate::builtin::results::BuiltInResults;
use crate::interpreter::values::ConstValue;
use crate::interpreter::{Interpreter, InterpreterCache, InterpreterContext};
use crate::TyChecker;
use crate::{BuiltinFunctionCtx, InferTy};
use boron_hir::item::VariantKind;
use boron_hir::{Enum, Variant};
use boron_parser::InterpreterMode;
use boron_resolver::{DefId, DefKind};
use boron_source::ident_table::get_or_intern;
use boron_target::abi::layout::Alignment;

pub fn align_of_ty<'a>(sz: &BuiltinFunctionCtx<'a>, ty: &InferTy) -> Alignment {
  let target = sz.sess.target();

  match ty {
    InferTy::Primitive(p, _) => target.size_of(*p).into(),
    InferTy::Ptr { .. } => target.pointer_width.size_bytes().into(),
    InferTy::Array { ty, .. } => align_of_ty(sz, ty),
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
  sz: &BuiltinFunctionCtx<'a>,
  def_id: &DefId,
  args: &Vec<InferTy>,
) -> Alignment {
  substituted_struct_field_tys(sz, def_id, args)
    .iter()
    .fold(0, |acc, field_ty| acc.max(align_of_ty(sz, field_ty).get()))
    .into()
}

pub(crate) fn substituted_struct_field_tys<'a>(
  sz: &BuiltinFunctionCtx<'a>,
  def_id: &DefId,
  args: &Vec<InferTy>,
) -> Vec<InferTy> {
  let ty_scheme = sz.ty_table.def_type(*def_id).unwrap();
  let (_, subst) = TyChecker::instantiate_with_args(&ty_scheme, args.as_slice());
  let strct = sz.hir.get_struct(*def_id).unwrap();

  strct
    .fields
    .iter()
    .map(|field| {
      let field_ty = sz.ty_table.field_type(*def_id, field.name).unwrap();
      TyChecker::apply_subst(&field_ty, &subst)
    })
    .collect()
}

pub fn calculate_enum_alignment<'a>(
  sz: &BuiltinFunctionCtx<'a>,
  def_id: &DefId,
  args: &Vec<InferTy>,
) -> Alignment {
  let e = sz.hir.get_enum(*def_id).unwrap();
  let tag_size = compute_discriminant_tag_size(sz, &e.variants);

  let max_field_align = substituted_enum_variant_field_tys(sz, def_id, &e, args)
    .iter()
    .flat_map(|fields| fields.iter())
    .fold(tag_size, |acc, field_ty| acc.max(align_of_ty(sz, field_ty).get()));

  max_field_align.into()
}

pub(crate) fn substituted_enum_variant_field_tys<'a>(
  sz: &BuiltinFunctionCtx<'a>,
  def_id: &DefId,
  e: &Enum,
  args: &Vec<InferTy>,
) -> Vec<Vec<InferTy>> {
  let ty_scheme = sz.ty_table.def_type(*def_id).unwrap();
  let (_, subst) = TyChecker::instantiate_with_args(&ty_scheme, args.as_slice());

  e.variants
    .iter()
    .map(|variant| match &variant.kind {
      VariantKind::Unit | VariantKind::Discriminant(..) => vec![],
      VariantKind::Struct(fields) => fields
        .iter()
        .map(|field| {
          let field_ty = sz.ty_table.field_type(variant.def_id, field.name).unwrap();
          TyChecker::apply_subst(&field_ty, &subst)
        })
        .collect(),
      VariantKind::Tuple(types) => types
        .iter()
        .enumerate()
        .map(|(i, _)| {
          let field_ty = sz
            .ty_table
            .field_type(variant.def_id, get_or_intern(&i.to_string(), None))
            .unwrap();
          TyChecker::apply_subst(&field_ty, &subst)
        })
        .collect(),
    })
    .collect()
}

pub fn compute_discriminant_tag_size<'a>(
  ctx: &BuiltinFunctionCtx<'a>,
  variants: &[Variant],
) -> usize {
  let discriminants = compute_variant_discriminants(ctx, variants);
  let max_discr = discriminants.into_iter().max().unwrap_or(0);
  discriminant_size_for_value(max_discr)
}

pub fn compute_variant_discriminants<'a>(
  ctx: &BuiltinFunctionCtx<'a>,
  variants: &[Variant],
) -> Vec<u128> {
  let cache = InterpreterCache::new();
  let results = BuiltInResults::new();
  let interpreter = Interpreter::new(
    ctx.sess.dcx(),
    &cache,
    ctx.resolver,
    ctx.hir,
    &results,
    InterpreterMode::Const,
    InterpreterContext::EnumDiscriminant,
  );

  let mut current: u128 = 0;
  let mut discriminants = Vec::with_capacity(variants.len());

  for variant in variants {
    match &variant.kind {
      VariantKind::Discriminant(expr) => {
        if let ConstValue::Int(v) = interpreter.evaluate_expr(expr) {
          current = v as u128;
        }
      }
      _ => {}
    }
    discriminants.push(current);
    current += 1;
  }

  discriminants
}

fn discriminant_size_for_value(max_value: u128) -> usize {
  if max_value <= u8::MAX as u128 {
    1
  } else if max_value <= u16::MAX as u128 {
    2
  } else if max_value <= u32::MAX as u128 {
    4
  } else {
    8
  }
}
