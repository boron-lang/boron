use crate::{Ir, IrFunction, IrId, IrStruct, SymbolMangler};
use boron_analysis::ty::SubstitutionMap;
use boron_analysis::{InferTy, TypeTable};
use boron_hir::{Hir, ParamKind, SemanticTy};
use boron_thir::{Function as ThirFunction, Struct as ThirStruct, Thir};

#[derive(Debug)]
pub struct IrLowerer<'a> {
  hir: &'a Hir,
  thir: &'a Thir,
  type_table: &'a TypeTable,
  ir: Ir,
  mangler: SymbolMangler<'a>,
}

impl<'a> IrLowerer<'a> {
  pub fn new(hir: &'a Hir, thir: &'a Thir, type_table: &'a TypeTable) -> Self {
    Self { hir, thir, type_table, ir: Ir::default(), mangler: SymbolMangler::new(hir) }
  }

  pub fn mangler(&self) -> &SymbolMangler<'a> {
    &self.mangler
  }

  pub fn lower(&mut self) -> Ir {
    for func in &self.thir.functions {
      self.lower_function(func.value());
    }

    for strukt in &self.thir.structs {
      self.lower_struct(strukt.value());
    }

    self.ir.clone()
  }

  pub fn lower_function(&mut self, func: &ThirFunction) {
    let scheme = self.type_table.def_type(func.def_id).unwrap();
    let is_generic = !scheme.vars.is_empty();

    if let Some(monomorphizations) = self.type_table.monomorphizations.get(&func.def_id) {
      for mono in monomorphizations.iter() {
        if mono.type_args.map().iter().any(|(_, ty)| ty.has_params()) {
          continue;
        }

        let params = self.lower_function_params(func, &mono.type_args);
        let type_args: Vec<SemanticTy> = scheme
          .vars
          .iter()
          .filter_map(|param| mono.type_args.get(param.def_id).map(Self::lower_type))
          .collect();

        let mangled_name = self.mangler.mangle_function(func.def_id, &type_args);

        self.ir.functions.push(IrFunction {
          name: mangled_name,
          params,
          id: IrId::new(),
          type_args,
        });
      }
    } else if !is_generic {
      let params = self.lower_function_params(func, &SubstitutionMap::new());
      let mangled_name = self.mangler.mangle_function(func.def_id, &[]);
      self.ir.functions.push(IrFunction {
        name: mangled_name,
        params,
        id: IrId::new(),
        type_args: vec![],
      });
    }
  }

  fn lower_function_params(
    &self,
    func: &ThirFunction,
    type_args: &SubstitutionMap,
  ) -> Vec<(String, SemanticTy)> {
    func
      .params
      .iter()
      .map(|p| {
        // Get param name from HIR (THIR params don't have names yet)
        let name = self
          .hir
          .get_function(func.def_id)
          .and_then(|hir_func| {
            hir_func.params.iter().find(|hp| hp.hir_id == p.hir_id).map(|hp| {
              match &hp.kind {
                ParamKind::Regular { name, .. } => name.text(),
                ParamKind::Variadic { name, .. } => name.text(),
                ParamKind::SelfParam { .. } => "self".to_string(),
              }
            })
          })
          .unwrap_or_else(|| format!("param_{}", p.def_id.index()));

        // Use type directly from THIR param
        let substituted_ty = Self::apply_subst_by_def_id(&p.ty, type_args);
        (name, Self::lower_type(&substituted_ty))
      })
      .collect()
  }

  pub fn lower_struct(&mut self, strukt: &ThirStruct) {
    let scheme = self.type_table.def_type(strukt.def_id).unwrap();
    let is_generic = !scheme.vars.is_empty();

    if let Some(monomorphizations) = self.type_table.monomorphizations.get(&strukt.def_id)
    {
      for mono in monomorphizations.iter() {
        if mono.type_args.map().iter().any(|(_, ty)| ty.has_params()) {
          continue;
        }

        let fields = self.lower_struct_fields(strukt, &mono.type_args);
        let type_args: Vec<SemanticTy> = scheme
          .vars
          .iter()
          .filter_map(|param| mono.type_args.get(param.def_id).map(Self::lower_type))
          .collect();

        let mangled_name = self.mangler.mangle_struct(strukt.def_id, &type_args);

        self.ir.structs.push(IrStruct {
          name: mangled_name,
          fields,
          id: IrId::new(),
          type_args,
        });
      }
    } else if !is_generic {
      let fields = self.lower_struct_fields(strukt, &SubstitutionMap::new());
      let mangled_name = self.mangler.mangle_struct(strukt.def_id, &[]);
      self.ir.structs.push(IrStruct {
        name: mangled_name,
        fields,
        id: IrId::new(),
        type_args: vec![],
      });
    }
  }

  fn lower_struct_fields(
    &self,
    strukt: &ThirStruct,
    type_args: &SubstitutionMap,
  ) -> Vec<(String, SemanticTy)> {
    strukt
      .fields
      .iter()
      .map(|f| {
        // Use type directly from THIR field
        let substituted_ty = Self::apply_subst_by_def_id(&f.ty, type_args);
        (f.name.text(), Self::lower_type(&substituted_ty))
      })
      .collect()
  }

  fn lower_type(ty: &InferTy) -> SemanticTy {
    match ty {
      InferTy::Primitive(kind, _) => SemanticTy::Primitive(*kind),

      InferTy::Adt { def_id, args, .. } => {
        let fields = args.iter().map(Self::lower_type).collect();
        SemanticTy::Struct { def_id: *def_id, fields }
      }

      InferTy::Ptr { mutability, ty: inner, .. } => SemanticTy::Ptr {
        mutability: *mutability,
        inner: Box::new(Self::lower_type(inner)),
      },

      InferTy::Optional(inner, _) => {
        SemanticTy::Optional(Box::new(Self::lower_type(inner)))
      }

      InferTy::Array { ty: inner, len, .. } => {
        SemanticTy::Array { elem: Box::new(Self::lower_type(inner)), len: *len }
      }

      InferTy::Slice(inner, _) => SemanticTy::Slice(Box::new(Self::lower_type(inner))),

      InferTy::Tuple(tys, _) => {
        SemanticTy::Tuple(tys.iter().map(Self::lower_type).collect())
      }

      InferTy::Fn { params, ret, .. } => SemanticTy::Fn {
        params: params.iter().map(Self::lower_type).collect(),
        ret: Box::new(Self::lower_type(ret)),
      },

      InferTy::Unit(_) => SemanticTy::Unit,
      InferTy::Never(_) => SemanticTy::Never,

      InferTy::Var(_, _) | InferTy::Param(_) => SemanticTy::Error,

      InferTy::Err(_) => SemanticTy::Error,
    }
  }

  fn apply_subst_by_def_id(ty: &InferTy, type_args: &SubstitutionMap) -> InferTy {
    match ty {
      InferTy::Var(_var, _span) => ty.clone(),
      InferTy::Param(param) => {
        // If we have a substitution for this param, use it; otherwise keep the param
        if let Some(subst_ty) = type_args.get(param.def_id) {
          subst_ty.clone()
        } else {
          ty.clone()
        }
      }
      InferTy::Adt { def_id, args, span } => InferTy::Adt {
        def_id: *def_id,
        args: args.iter().map(|t| Self::apply_subst_by_def_id(t, type_args)).collect(),
        span: *span,
      },
      InferTy::Ptr { mutability, ty: inner, span } => InferTy::Ptr {
        mutability: *mutability,
        ty: Box::new(Self::apply_subst_by_def_id(inner, type_args)),
        span: *span,
      },
      InferTy::Optional(inner, span) => {
        InferTy::Optional(Box::new(Self::apply_subst_by_def_id(inner, type_args)), *span)
      }
      InferTy::Array { ty: inner, len, span } => InferTy::Array {
        ty: Box::new(Self::apply_subst_by_def_id(inner, type_args)),
        len: *len,
        span: *span,
      },
      InferTy::Slice(inner, span) => {
        InferTy::Slice(Box::new(Self::apply_subst_by_def_id(inner, type_args)), *span)
      }
      InferTy::Tuple(tys, span) => InferTy::Tuple(
        tys.iter().map(|t| Self::apply_subst_by_def_id(t, type_args)).collect(),
        *span,
      ),
      InferTy::Fn { params, ret, span } => InferTy::Fn {
        params: params
          .iter()
          .map(|t| Self::apply_subst_by_def_id(t, type_args))
          .collect(),
        ret: Box::new(Self::apply_subst_by_def_id(ret, type_args)),
        span: *span,
      },
      _ => ty.clone(),
    }
  }
}
