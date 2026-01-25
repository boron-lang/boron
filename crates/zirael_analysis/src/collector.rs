use crate::{InferTy, TyChecker, TypeScheme};
use zirael_hir::{Function, Param, ParamKind};

impl TyChecker<'_> {
  fn function_signature(&self, func: &Function) -> TypeScheme {
    let ty_vars = self.register_generics(&func.generics);

    let params = self.param_types(&func.params);
    let ret = self.lower_hir_ty(&func.return_type);

    let fn_ty = InferTy::Fn { params, ret: Box::new(ret), span: func.span };

    TypeScheme { vars: ty_vars, ty: fn_ty }
  }

  fn param_types(&self, params: &Vec<Param>) -> Vec<InferTy> {
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

      let ty_vars = self.register_generics(&strukt.generics);

      for field in &strukt.fields {
        let field_ty = self.lower_hir_ty(&field.ty);
        self.table.record_field_type(def_id, field.name.text(), field_ty);
      }

      let struct_ty = InferTy::Adt {
        def_id,
        args: ty_vars.iter().map(|&v| InferTy::Var(v, strukt.span)).collect(),
        span: strukt.span,
      };
      self
        .table
        .record_def_type(def_id, TypeScheme { vars: ty_vars.clone(), ty: struct_ty });
    }

    for entry in &self.hir.enums {
      let def_id = *entry.key();
      let eenum = entry.value();

      let ty_vars = self.register_generics(&eenum.generics);

      let enum_ty = InferTy::Adt {
        def_id,
        args: ty_vars.iter().map(|&v| InferTy::Var(v, eenum.span)).collect(),
        span: eenum.span,
      };
      self.table.record_def_type(def_id, TypeScheme { vars: ty_vars, ty: enum_ty });

      // TODO: Record enum variant types
    }

    for entry in &self.hir.consts {
      let def_id = *entry.key();
      let konst = entry.value();
      self.infcx.clear_type_params();
      let ty = self.lower_hir_ty(&konst.ty);
      self.table.record_def_type(def_id, TypeScheme::mono(ty));
    }
  }
}
