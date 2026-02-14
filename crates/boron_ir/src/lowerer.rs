use crate::{
  Ir, IrExpr, IrExprKind, IrFieldInit, IrFunction, IrId, IrParam, IrStruct, Projection,
  SymbolMangler, cfg::lowerer::CfgLoweringContext,
};
use boron_analysis::ty::SubstitutionMap;
use boron_analysis::{InferTy, TypeScheme, TypeTable};
use boron_hir::pat::PatKind;
use boron_hir::{Hir, ParamKind, Pat, SemanticTy};
use boron_session::prelude::debug;
use boron_thir::{
  Expr as ThirExpr, ExprKind as ThirExprKind, FieldInit as ThirFieldInit,
  Function as ThirFunction, Struct as ThirStruct, Thir,
};
use itertools::Itertools as _;

#[derive(Debug)]
pub struct IrLowerer<'a> {
  hir: &'a Hir,
  thir: &'a Thir,
  type_table: &'a TypeTable,
  pub ir: Ir,
  mangler: SymbolMangler<'a>,
  pub current_function: IrId,
  cfg_context: Option<CfgLoweringContext>,
}

impl<'a> IrLowerer<'a> {
  pub fn new(hir: &'a Hir, thir: &'a Thir, type_table: &'a TypeTable) -> Self {
    Self {
      hir,
      thir,
      type_table,
      ir: Ir::default(),
      mangler: SymbolMangler::new(hir),
      current_function: IrId::dummy(),
      cfg_context: None,
    }
  }

  pub fn mangler(&self) -> &SymbolMangler<'a> {
    &self.mangler
  }

  pub(crate) fn set_cfg_context(&mut self, ctx: CfgLoweringContext) {
    self.cfg_context = Some(ctx);
  }

  pub(crate) fn clear_cfg_context(&mut self) {
    self.cfg_context = None;
  }

  pub(crate) fn cfg_context(&self) -> &CfgLoweringContext {
    self.cfg_context.as_ref().expect("cfg lowering context is missing")
  }

  pub(crate) fn cfg_context_mut(&mut self) -> &mut CfgLoweringContext {
    self.cfg_context.as_mut().expect("cfg lowering context is missing")
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
    let id = IrId::new();

    self.current_function = id;
    let Some(monomorphizations) = self.type_table.monomorphizations.get(&func.def_id)
    else {
      if scheme.vars.is_empty() {
        let empty_subst = SubstitutionMap::new();
        self.push_function_instance(func, &scheme, &empty_subst, id);
      }
      self.current_function = IrId::dummy();
      return;
    };

    for mono in monomorphizations
      .iter()
      .filter(|mono| !mono.type_args.map().iter().any(|(_, ty)| ty.has_params()))
    {
      self.push_function_instance(func, &scheme, &mono.type_args, id);
    }

    self.current_function = IrId::dummy();
  }

  fn push_function_instance(
    &mut self,
    func: &ThirFunction,
    scheme: &TypeScheme,
    type_args: &SubstitutionMap,
    id: IrId,
  ) {
    let params = self.lower_function_params(func, type_args);
    let concrete_type_args = Self::collect_type_args(scheme, type_args);
    let mangled_name = self.mangler.mangle_function(func.def_id, &concrete_type_args);
    let return_ty = Self::apply_subst_by_def_id(&func.return_type, type_args);
    let body = func.body.as_ref().map(|b| self.lower_body(b, type_args, func.def_id));

    self.ir.functions.push(IrFunction {
      name: mangled_name,
      params,
      id,
      type_args: concrete_type_args,
      return_type: Self::lower_type(&return_ty),
      def_id: func.def_id,
      body,
    });
  }

  fn lower_function_params(
    &self,
    func: &ThirFunction,
    type_args: &SubstitutionMap,
  ) -> Vec<IrParam> {
    let hir_func = self.hir.get_function(func.def_id);
    func
      .params
      .iter()
      .map(|p| {
        let name = hir_func
          .as_ref()
          .and_then(|hir_func| hir_func.params.iter().find(|hp| hp.hir_id == p.hir_id))
          .map(|hp| match &hp.kind {
            ParamKind::Regular { name, .. } | ParamKind::Variadic { name, .. } => {
              name.text()
            }
            ParamKind::SelfParam { .. } => "self".to_owned(),
          })
          .unwrap_or_else(|| format!("param_{}", p.def_id.index()));

        let substituted_ty = Self::apply_subst_by_def_id(&p.ty, type_args);
        IrParam { def_id: p.def_id, name, ty: Self::lower_type(&substituted_ty) }
      })
      .collect()
  }

  pub fn lower_struct(&mut self, strukt: &ThirStruct) {
    let scheme = self.type_table.def_type(strukt.def_id).unwrap();
    let Some(monomorphizations) = self.type_table.monomorphizations.get(&strukt.def_id)
    else {
      if scheme.vars.is_empty() {
        let empty_subst = SubstitutionMap::new();
        self.push_struct_instance(strukt, &scheme, &empty_subst);
      }
      return;
    };

    for mono in monomorphizations
      .iter()
      .filter(|mono| !mono.type_args.map().iter().any(|(_, ty)| ty.has_params()))
    {
      self.push_struct_instance(strukt, &scheme, &mono.type_args);
    }
  }

  fn push_struct_instance(
    &mut self,
    strukt: &ThirStruct,
    scheme: &TypeScheme,
    type_args: &SubstitutionMap,
  ) {
    let fields = Self::lower_struct_fields(strukt, type_args);
    let concrete_type_args = Self::collect_type_args(scheme, type_args);
    let mangled_name = self.mangler.mangle_struct(strukt.def_id, &concrete_type_args);

    self.ir.structs.push(IrStruct {
      name: mangled_name,
      fields,
      id: IrId::new(),
      type_args: concrete_type_args,
      def_id: strukt.def_id,
    });
  }

  fn collect_type_args(
    scheme: &TypeScheme,
    type_args: &SubstitutionMap,
  ) -> Vec<SemanticTy> {
    scheme
      .vars
      .iter()
      .filter_map(|param| type_args.get(param.def_id).map(Self::lower_type))
      .collect()
  }

  fn lower_struct_fields(
    strukt: &ThirStruct,
    type_args: &SubstitutionMap,
  ) -> Vec<(String, SemanticTy)> {
    strukt
      .fields
      .iter()
      .map(|f| {
        let substituted_ty = Self::apply_subst_by_def_id(&f.ty, type_args);
        (f.name.text(), Self::lower_type(&substituted_ty))
      })
      .collect()
  }

  pub fn lower_local_pattern(
    &self,
    local_ty: &SemanticTy,
    pat: &Pat,
    projections: &mut Vec<Projection>,
  ) {
    match &pat.kind {
      PatKind::Binding { subpat, def_id, .. } => {
        projections.push(Projection::Binding(*def_id));

        if let Some(subpat) = subpat {
          self.lower_local_pattern(local_ty, subpat, projections);
        }
      }
      PatKind::Struct { fields, def_id, rest: _ } => {
        let struct_fields = self.hir.get_struct(*def_id).unwrap();

        for field in fields {
          let Some((field_idx, _)) =
            struct_fields.fields.iter().find_position(|f| f.name == field.name)
          else {
            unreachable!("checked befores")
          };

          let def_id = if let PatKind::Binding { def_id, .. } = &field.pat.kind {
            Some(*def_id)
          } else {
            debug!("non-binding pattern in struct field");
            None
          };

          projections.push(Projection::Field {
            struct_ty: local_ty.clone(),
            field_idx: field_idx as u32,
            def_id,
          });
        }
      }
      _ => todo!("lower complex pattern bindings in IR"),
    }
  }

  pub fn lower_semantic_ty(ty: &InferTy, type_args: &SubstitutionMap) -> SemanticTy {
    let substituted = Self::apply_subst_by_def_id(ty, type_args);
    Self::lower_type(&substituted)
  }

  fn lower_type(ty: &InferTy) -> SemanticTy {
    match ty {
      InferTy::Primitive(kind, _) => SemanticTy::Primitive(*kind),

      InferTy::Adt { def_id, args, .. } => {
        let args = args.iter().map(Self::lower_type).collect();
        SemanticTy::Struct { def_id: *def_id, args }
      }

      InferTy::Ptr { mutability, ty: inner, .. } => SemanticTy::Ptr {
        mutability: *mutability,
        inner: Box::new(Self::lower_type(inner)),
      },

      InferTy::Optional(inner, _) => {
        SemanticTy::Optional(Box::new(Self::lower_type(inner)))
      }

      InferTy::Array { ty: inner, len, .. } => SemanticTy::Array {
        elem: Box::new(Self::lower_type(inner)),
        len: len.expect_len(),
      },

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

      InferTy::Var(_, _) | InferTy::Param(_) | InferTy::Err(_) => SemanticTy::Error,
    }
  }

  fn apply_subst_by_def_id(ty: &InferTy, type_args: &SubstitutionMap) -> InferTy {
    match ty {
      InferTy::Var(_var, _span) => ty.clone(),
      InferTy::Param(param) => {
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
