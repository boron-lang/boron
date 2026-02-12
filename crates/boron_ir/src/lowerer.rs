use crate::{
  Ir, IrExpr, IrExprKind, IrFieldInit, IrFunction, IrId, IrParam, IrStruct, Projection,
  SymbolMangler,
};
use boron_analysis::ty::SubstitutionMap;
use boron_analysis::{InferTy, TypeTable};
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
    }
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
    let id = IrId::new();

    self.current_function = id;
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
        let return_ty = Self::apply_subst_by_def_id(&func.return_type, &mono.type_args);

        let body =
          func.body.as_ref().map(|b| self.lower_body(b, &mono.type_args, func.def_id));

        self.ir.functions.push(IrFunction {
          name: mangled_name,
          params,
          id,
          type_args,
          return_type: Self::lower_type(&return_ty),
          def_id: func.def_id,
          body,
        });
      }
    } else if !is_generic {
      let params = self.lower_function_params(func, &SubstitutionMap::new());
      let mangled_name = self.mangler.mangle_function(func.def_id, &[]);
      let body = func
        .body
        .as_ref()
        .map(|b| self.lower_body(b, &SubstitutionMap::new(), func.def_id));
      self.ir.functions.push(IrFunction {
        name: mangled_name,
        params,
        id,
        type_args: vec![],
        return_type: Self::lower_type(&func.return_type),
        def_id: func.def_id,
        body,
      });
    }

    self.current_function = IrId::dummy();
  }

  fn lower_function_params(
    &self,
    func: &ThirFunction,
    type_args: &SubstitutionMap,
  ) -> Vec<IrParam> {
    func
      .params
      .iter()
      .map(|p| {
        let name = self
          .hir
          .get_function(func.def_id)
          .and_then(|hir_func| {
            hir_func.params.iter().find(|hp| hp.hir_id == p.hir_id).map(|hp| {
              match &hp.kind {
                ParamKind::Regular { name, .. } => name.text(),
                ParamKind::Variadic { name, .. } => name.text(),
                ParamKind::SelfParam { .. } => "self".to_owned(),
              }
            })
          })
          .unwrap_or_else(|| format!("param_{}", p.def_id.index()));

        let substituted_ty = Self::apply_subst_by_def_id(&p.ty, type_args);
        IrParam { def_id: p.def_id, name, ty: Self::lower_type(&substituted_ty) }
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
          def_id: strukt.def_id,
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
        def_id: strukt.def_id,
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
      PatKind::Struct { fields, def_id, rest } => {
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

  pub(crate) fn lower_expr(
    &self,
    expr: &ThirExpr,
    type_args: &SubstitutionMap,
  ) -> IrExpr {
    let kind = match &expr.kind {
      ThirExprKind::Literal(lit) => IrExprKind::Literal(lit.clone()),
      ThirExprKind::LocalRef(def_id) => IrExprKind::LocalRef(*def_id),
      ThirExprKind::Path(def_id) => IrExprKind::GlobalRef(*def_id),
      ThirExprKind::Binary { op, lhs, rhs } => IrExprKind::Binary {
        op: *op,
        lhs: Box::new(self.lower_expr(lhs, type_args)),
        rhs: Box::new(self.lower_expr(rhs, type_args)),
      },
      ThirExprKind::Unary { op, operand } => IrExprKind::Unary {
        op: *op,
        operand: Box::new(self.lower_expr(operand, type_args)),
      },
      ThirExprKind::Assign { target, value } => IrExprKind::Assign {
        target: Box::new(self.lower_expr(target, type_args)),
        value: Box::new(self.lower_expr(value, type_args)),
      },
      ThirExprKind::Cast { expr: inner, ty } => IrExprKind::Cast {
        expr: Box::new(self.lower_expr(inner, type_args)),
        ty: self.lower_semantic_ty(ty, type_args),
      },
      ThirExprKind::Call { callee, type_args: call_type_args, args } => {
        IrExprKind::Call {
          callee: *callee,
          type_args: call_type_args
            .iter()
            .map(|ty| self.lower_semantic_ty(ty, type_args))
            .collect(),
          args: args.iter().map(|a| self.lower_expr(a, type_args)).collect(),
        }
      }
      ThirExprKind::Field { object, field } => IrExprKind::Field {
        object: Box::new(self.lower_expr(object, type_args)),
        field: *field,
      },
      ThirExprKind::Index { object, index } => IrExprKind::Index {
        object: Box::new(self.lower_expr(object, type_args)),
        index: Box::new(self.lower_expr(index, type_args)),
      },
      ThirExprKind::AddrOf { operand } => {
        IrExprKind::AddrOf { operand: Box::new(self.lower_expr(operand, type_args)) }
      }
      ThirExprKind::Struct { def_id, type_args: struct_type_args, fields } => {
        IrExprKind::Struct {
          def_id: *def_id,
          type_args: struct_type_args
            .iter()
            .map(|ty| self.lower_semantic_ty(ty, type_args))
            .collect(),
          fields: fields.iter().map(|f| self.lower_field_init(f, type_args)).collect(),
        }
      }
      ThirExprKind::Tuple(exprs) => {
        IrExprKind::Tuple(exprs.iter().map(|e| self.lower_expr(e, type_args)).collect())
      }
      ThirExprKind::Array(exprs) => {
        IrExprKind::Array(exprs.iter().map(|e| self.lower_expr(e, type_args)).collect())
      }
      ThirExprKind::Block(_) => todo!("lower block expressions into CFG"),
      ThirExprKind::If { .. } => todo!("lower if into CFG terminators"),
      ThirExprKind::Match { .. } => todo!("lower match into CFG"),
      ThirExprKind::Loop { .. } => todo!("lower loops into CFG"),
      ThirExprKind::Break { .. } => todo!("lower break into CFG"),
      ThirExprKind::Continue => todo!("lower continue into CFG"),
      ThirExprKind::Return { .. } => todo!("lower return into CFG terminator"),
      ThirExprKind::Err => IrExprKind::Err,
    };

    IrExpr {
      hir_id: expr.hir_id,
      ty: self.lower_semantic_ty(&expr.ty, type_args),
      kind,
      span: expr.span,
    }
  }

  fn lower_field_init(
    &self,
    field: &ThirFieldInit,
    type_args: &SubstitutionMap,
  ) -> IrFieldInit {
    let ty = self.lower_semantic_ty(&field.ty, type_args);

    IrFieldInit {
      hir_id: field.hir_id,
      name: field.name.text(),
      ty,
      value: self.lower_expr(&field.value, type_args),
      span: field.span,
    }
  }

  pub fn lower_semantic_ty(
    &self,
    ty: &InferTy,
    type_args: &SubstitutionMap,
  ) -> SemanticTy {
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

      InferTy::Var(_, _) | InferTy::Param(_) => SemanticTy::Error,

      InferTy::Err(span) => SemanticTy::Error,
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
