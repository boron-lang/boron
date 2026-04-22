use crate::{InferTy, TyChecker, TypeScheme};
use boron_source::ident_table::get_or_intern;
use boron_source::span::Span;
use boron_types::hir::{AdtEntry, Function, Generics, Param, ParamKind, VariantKind};

impl TyChecker<'_> {
  fn function_signature(&self, func: &Function) -> TypeScheme {
    let generics = if let Some(parent) = self.ctx.adt_parent(func.def_id) {
      let mut generics = Generics { span: Span::dummy(), params: vec![] };

      if let Some(parent_generics) = self.ctx.adt_generics(parent) {
        generics.params.extend(parent_generics.params);
      }

      generics.params.extend(func.generics.params.clone());

      generics
    } else {
      func.generics.clone()
    };

    let ty_vars = self.register_generics(&generics);
    let params = self.param_types(&func.params);
    let ret = self.lower_hir_ty(&func.return_type);

    let fn_ty = InferTy::Fn { params, ret: Box::new(ret), span: func.span };
    TypeScheme { vars: ty_vars, ty: fn_ty }
  }

  fn param_types(&self, params: &[Param]) -> Vec<InferTy> {
    params
      .iter()
      .filter_map(|p| match &p.kind {
        ParamKind::Regular { ty, .. } | ParamKind::Variadic { ty, .. } => {
          Some(self.lower_hir_ty(ty))
        }
        ParamKind::SelfParam { .. } => None,
      })
      .collect()
  }

  pub fn collect_signatures(&self) {
    for entry in &self.hir().functions {
      let def_id = *entry.key();
      let func = entry.value();
      let scheme = self.function_signature(func);
      self.ctx.record_def_type(def_id, scheme);
    }

    for adt in &self.hir().adts {
      match adt.value() {
        AdtEntry::Struct(strukt) => {
          let def_id = strukt.def_id;

          let generics = self.register_generics(&strukt.generics);
          for field in &strukt.fields {
            let field_ty = self.lower_hir_ty(&field.ty);
            self.ctx.record_field_type(def_id, field.name, field_ty);
          }

          let struct_ty = InferTy::Adt {
            def_id,
            args: generics.iter().map(|&g| InferTy::Param(g)).collect(),
            span: strukt.span,
          };

          self.ctx.record_def_type(def_id, TypeScheme { vars: generics, ty: struct_ty });
        }
        AdtEntry::Enum(_enum) => {
          let def_id = _enum.def_id;

          let generics = self.register_generics(&_enum.generics);
          for variant in &_enum.variants {
            match &variant.kind {
              VariantKind::Unit | VariantKind::Discriminant(..) => {}
              VariantKind::Struct(fields) => {
                for field in fields {
                  let field_ty = self.lower_hir_ty(&field.ty);
                  self.ctx.record_field_type(variant.def_id, field.name, field_ty);
                }
              }
              VariantKind::Tuple(types) => {
                for (field, ty) in types.iter().enumerate() {
                  let field_ty = self.lower_hir_ty(ty);
                  self.ctx.record_field_type(
                    variant.def_id,
                    get_or_intern(&field.to_string(), None),
                    field_ty,
                  );
                }
              }
            }
          }

          let struct_ty = InferTy::Adt {
            def_id,
            args: generics.iter().map(|&g| InferTy::Param(g)).collect(),
            span: _enum.span,
          };

          self.ctx.record_def_type(def_id, TypeScheme { vars: generics, ty: struct_ty });
        }
      }
    }

    for entry in &self.hir().consts {
      let def_id = *entry.key();
      let konst = entry.value();
      let ty = self.lower_hir_ty(&konst.ty);
      self.ctx.record_def_type(def_id, TypeScheme::mono(ty));
    }
  }
}
