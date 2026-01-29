use std::collections::HashMap;
use dashmap::DashMap;
use crate::ty::GenericId;
use crate::{InferTy, TyChecker, TyVar, TypeScheme};
use boron_hir::{Function, GenericParamKind, Generics, Param, ParamKind};
use boron_resolver::DefId;
use boron_source::span::Span;

impl TyChecker<'_> {
  fn function_signature(&self, func: &Function) -> TypeScheme {
    let generics = if let Some(strukt) = self.hir.is_struct_child(&func.def_id) {
      let mut generics = Generics { span: Span::dummy(), params: vec![] };
      generics.params.extend(strukt.generics.params.clone());
      generics.params.extend(func.generics.params.clone());

      generics
    } else {
      func.generics.clone()
    };

    let ty_vars = self.register_generics(&generics);
    let map = generics.params.iter()
        .zip(&ty_vars)
        .map(|(p, &v)| (p.def_id, v))
        .collect();

    let params = self.param_types(&func.params, &map);
    let ret = self.lower_hir_ty(&func.return_type);

    let fn_ty = InferTy::Fn { params, ret: Box::new(ret), span: func.span };
    TypeScheme { vars: ty_vars, ty: fn_ty, map }
  }

  fn param_types(&self, params: &Vec<Param>, vars: &DashMap<DefId, TyVar>) -> Vec<InferTy> {
    params
      .iter()
      .filter_map(|p| match &p.kind {
        ParamKind::Regular { ty, .. } => Some(self.lower_hir_ty(ty)),
        ParamKind::Variadic { ty, .. } => Some(self.lower_hir_ty(ty)),
        ParamKind::SelfParam { .. } => None,
      })
      .collect()
  }

  pub fn collect_signatures(&mut self) {
    for entry in &self.hir.functions {
      let def_id = *entry.key();
      let func = entry.value();
      let scheme = self.function_signature(func);
      self.table.record_def_type(def_id, scheme);
    }

    for entry in &self.hir.structs {
      let def_id = *entry.key();
      let strukt = entry.value();

      let generics = self.register_generics(&strukt.generics);
      let map = strukt.generics.params.iter()
          .zip(&generics)
          .map(|(p, &v)| (p.def_id, v))
          .collect();

      for field in &strukt.fields {
        let field_ty = self.lower_hir_ty(&field.ty);
        self.table.record_field_type(def_id, field.name.text(), field_ty);
      }

      let struct_ty = InferTy::Adt {
        def_id,
        args: generics.iter().map(|&g| InferTy::Var(g, Span::default())).collect(),
        span: strukt.span,
      };

      self.table.record_def_type(def_id, TypeScheme { vars: generics, ty: struct_ty, map });
    }

    for entry in &self.hir.enums {
      todo!()
    }

    for entry in &self.hir.consts {
      let def_id = *entry.key();
      let konst = entry.value();
      let ty = self.lower_hir_ty(&konst.ty);
      self.table.record_def_type(def_id, TypeScheme::mono(ty));
    }
  }
}
