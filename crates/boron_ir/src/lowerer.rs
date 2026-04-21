use crate::{
  IrBlock, IrBody, IrEnum, IrEnumVariant, IrExpr, IrExprKind, IrFieldInit, IrFunction,
  IrLocal, IrParam, IrStmt, IrStmtKind, IrStruct, Projection, SymbolMangler,
};
use boron_analysis::align_of::{
  calculate_enum_alignment, compute_variant_discriminants_from_exprs,
};
use boron_analysis::size_of::{calculate_enum_payload_layout, calculate_enum_size};
use boron_analysis::{BuiltinFunctionCtx, InferTy, TypeScheme, TypeTable};
use boron_resolver::{DefId, Resolver};
use boron_session::prelude::{debug, Session};
use boron_source::ident_table::get_or_intern;
use boron_target::abi::layout::Layout;
use boron_types::hir::{EnumVariant, Hir, Pat, PatKind, SemanticTy};
use boron_types::infer_ty::SubstitutionMap;
use boron_types::ir::{Ir, IrId};
use boron_types::thir::{
  Block as ThirBlock, Enum as ThirEnum, Expr as ThirExpr, ExprKind as ThirExprKind,
  FieldInit as ThirFieldInit, Function as ThirFunction, StmtKind, Struct as ThirStruct,
  Thir, VariantKind,
};
use itertools::Itertools as _;

#[derive(Debug)]
struct LoweringContext {
  owner: DefId,
  type_args: SubstitutionMap,
}

pub struct IrLowerer<'a> {
  hir: &'a Hir,
  thir: &'a Thir,
  type_table: &'a TypeTable,
  sess: &'a Session,
  resolver: &'a Resolver,
  mangler: SymbolMangler<'a>,
  lowering_context: Option<LoweringContext>,

  pub ir: Ir,
  pub current_function: IrId,
}

impl<'a> IrLowerer<'a> {
  pub fn new(
    hir: &'a Hir,
    thir: &'a Thir,
    type_table: &'a TypeTable,
    sess: &'a Session,
    resolver: &'a Resolver,
  ) -> Self {
    Self {
      hir,
      thir,
      type_table,
      sess,
      resolver,
      ir: Ir::default(),
      mangler: SymbolMangler::new(hir),
      current_function: IrId::dummy(),
      lowering_context: None,
    }
  }

  fn builtin_ctx(&self) -> BuiltinFunctionCtx<'a> {
    BuiltinFunctionCtx {
      sess: self.sess,
      resolver: self.resolver,
      hir: self.hir,
      ty_table: self.type_table,
    }
  }

  pub fn mangler(&self) -> &SymbolMangler<'a> {
    &self.mangler
  }

  fn context(&self) -> &LoweringContext {
    self.lowering_context.as_ref().expect("lowering context is missing")
  }

  pub fn lower(&mut self) -> Ir {
    for func in &self.thir.functions {
      self.lower_function(func.value());
    }

    for strukt in &self.thir.structs {
      self.lower_struct(strukt.value());
    }

    for enum_ in &self.thir.enums {
      self.lower_enum(enum_.value());
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
    let concrete_type_args = self.collect_type_args(scheme, type_args);
    let mangled_name = self.mangler.mangle_function(func.def_id, &concrete_type_args);
    let return_ty = Self::apply_subst_by_def_id(&func.return_type, type_args);
    let body = func.body.as_ref().map(|b| self.lower_body(b, type_args, func.def_id));

    self.ir.functions.push(IrFunction {
      name: mangled_name,
      params,
      id,
      type_args: concrete_type_args,
      return_type: self.lower_type(&return_ty),
      def_id: func.def_id,
      body,
    });
  }

  fn lower_function_params(
    &self,
    func: &ThirFunction,
    type_args: &SubstitutionMap,
  ) -> Vec<IrParam> {
    let hir_func = self.thir.get_function(&func.def_id);
    func
      .params
      .iter()
      .map(|p| {
        let name = hir_func
          .params
          .iter()
          .find(|hp| hp.hir_id == p.hir_id)
          .map(|hp| hp.name.to_string())
          .unwrap_or_else(|| format!("param_{}", p.def_id.index()));

        let substituted_ty = Self::apply_subst_by_def_id(&p.ty, type_args);
        IrParam { def_id: p.def_id, name, ty: self.lower_type(&substituted_ty) }
      })
      .collect()
  }

  pub(crate) fn lower_body(
    &mut self,
    block: &ThirBlock,
    type_args: &SubstitutionMap,
    owner: DefId,
  ) -> IrBody {
    self.lowering_context = Some(LoweringContext { owner, type_args: type_args.clone() });

    let ir_block = self.lower_block(block);

    self.lowering_context = None;

    IrBody { block: ir_block }
  }

  fn lower_block(&mut self, block: &ThirBlock) -> IrBlock {
    let mut stmts = Vec::new();

    for stmt in &block.stmts {
      match &stmt.kind {
        StmtKind::Local(local) => {
          let type_args = &self.context().type_args;
          let ty = self.lower_semantic_ty(&local.ty, type_args);
          let mut projections = vec![];
          let hir_id = local.hir_id;

          self.lower_local_pattern(&ty, &local.pat, &mut projections);

          let init = local.init.clone().unwrap();
          let local = IrLocal {
            hir_id: local.hir_id,
            ty,
            init: self.lower_expr(&init),
            span: local.span,
            projections,
          };
          self.ir.add_local(self.current_function, local);

          stmts.push(IrStmt {
            hir_id: stmt.hir_id,
            kind: IrStmtKind::Local(hir_id),
            span: stmt.span,
          });
        }
        StmtKind::Expr(expr) => {
          let lowered = self.lower_expr(expr);
          stmts.push(IrStmt {
            hir_id: stmt.hir_id,
            kind: IrStmtKind::Expr(lowered),
            span: stmt.span,
          });
        }
      }
    }

    let expr = block.expr.as_ref().map(|e| Box::new(self.lower_expr(e)));

    IrBlock { hir_id: block.hir_id, stmts, expr, span: block.span }
  }

  fn lower_expr(&mut self, expr: &ThirExpr) -> IrExpr {
    let type_args = &self.context().type_args;
    let ty = self.lower_semantic_ty(&expr.ty, type_args);

    let kind = match &expr.kind {
      ThirExprKind::Literal(lit) => IrExprKind::Literal(lit.clone()),
      ThirExprKind::LocalRef(def_id) => IrExprKind::LocalRef(*def_id),
      ThirExprKind::Path(def_id) => IrExprKind::Path(*def_id),

      ThirExprKind::Binary { op, lhs, rhs } => {
        let lhs = self.lower_expr(lhs);
        let rhs = self.lower_expr(rhs);
        IrExprKind::Binary { op: *op, lhs: Box::new(lhs), rhs: Box::new(rhs) }
      }
      ThirExprKind::Unary { op, operand } => {
        let operand = self.lower_expr(operand);
        IrExprKind::Unary { op: *op, operand: Box::new(operand) }
      }
      ThirExprKind::Assign { target, value } => {
        let target = self.lower_expr(target);
        let value = self.lower_expr(value);
        IrExprKind::Assign { target: Box::new(target), value: Box::new(value) }
      }
      ThirExprKind::Cast { expr: inner, ty } => {
        let inner = self.lower_expr(inner);
        let cast_ty = self.lower_semantic_ty(ty, &self.context().type_args);
        IrExprKind::Cast { expr: Box::new(inner), ty: cast_ty }
      }
      ThirExprKind::Call { callee, callee_name, type_args: call_type_args, args } => {
        let args = args.iter().map(|arg| self.lower_expr(arg)).collect();
        let type_args = call_type_args
          .iter()
          .map(|ty| self.lower_semantic_ty(ty, &self.context().type_args))
          .collect();
        IrExprKind::Call { callee: *callee, callee_name: *callee_name, type_args, args }
      }
      ThirExprKind::Field { object, field } => {
        let object = self.lower_expr(object);
        let field_idx = match &object.ty {
          SemanticTy::Struct { def_id, .. } => {
            let strukt = self.thir.get_struct(def_id);

            strukt
              .fields
              .iter()
              .position(|f| f.name == *field)
              .map(|idx| idx as u32)
              .expect("field should exist on struct")
          }
          SemanticTy::Tuple(_) => {
            field.text().parse::<u32>().expect("tuple field should be a numeric index")
          }
          _ => unreachable!("field access should only be lowered for struct/tuple"),
        };

        IrExprKind::Field { object: Box::new(object), field_idx }
      }
      ThirExprKind::Index { object, index } => {
        let object = self.lower_expr(object);
        let index = self.lower_expr(index);
        IrExprKind::Index { object: Box::new(object), index: Box::new(index) }
      }
      ThirExprKind::Struct { def_id, type_args: struct_type_args, fields } => {
        let fields = fields.iter().map(|f| self.lower_field_init(f)).collect();
        let type_args = struct_type_args
          .iter()
          .map(|ty| self.lower_semantic_ty(ty, &self.context().type_args))
          .collect();
        IrExprKind::Struct { def_id: *def_id, type_args, fields }
      }
      ThirExprKind::Tuple(exprs) => {
        IrExprKind::Tuple(exprs.iter().map(|e| self.lower_expr(e)).collect())
      }
      ThirExprKind::Array(exprs) => {
        IrExprKind::Array(exprs.iter().map(|e| self.lower_expr(e)).collect())
      }

      ThirExprKind::Block(block) => IrExprKind::Block(self.lower_block(block)),
      ThirExprKind::If { condition, then_block, else_branch } => {
        let condition = self.lower_expr(condition);
        let then_block = self.lower_block(then_block);
        let else_branch = else_branch.as_ref().map(|e| Box::new(self.lower_expr(e)));
        IrExprKind::If { condition: Box::new(condition), then_block, else_branch }
      }
      ThirExprKind::Loop { body } => IrExprKind::Loop { body: self.lower_block(body) },
      ThirExprKind::Break { value: _ } => IrExprKind::Break,
      ThirExprKind::Continue => IrExprKind::Continue,
      ThirExprKind::Return { value } => {
        let value = value.as_ref().map(|e| Box::new(self.lower_expr(e)));
        IrExprKind::Return { value }
      }
      ThirExprKind::Match { .. } => {
        todo!("lower match into IR");
      }
      ThirExprKind::Err => {
        panic!("err expr shouldn't exist at this point: {expr:#?}")
      }
    };

    IrExpr { hir_id: expr.hir_id, ty, kind, span: expr.span }
  }

  fn lower_field_init(&mut self, field: &ThirFieldInit) -> IrFieldInit {
    let type_args = &self.context().type_args;
    let ty = self.lower_semantic_ty(&field.ty, type_args);

    IrFieldInit {
      hir_id: field.hir_id,
      name: field.name.text(),
      ty,
      value: self.lower_expr(&field.value),
      span: field.span,
    }
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

  pub fn lower_enum(&mut self, enum_: &ThirEnum) {
    let scheme = self.type_table.def_type(enum_.def_id).unwrap();
    let Some(monomorphizations) = self.type_table.monomorphizations.get(&enum_.def_id)
    else {
      if scheme.vars.is_empty() {
        let empty_subst = SubstitutionMap::new();
        self.push_enum_instance(enum_, &scheme, &empty_subst);
      }
      return;
    };

    for mono in monomorphizations
      .iter()
      .filter(|mono| !mono.type_args.map().iter().any(|(_, ty)| ty.has_params()))
    {
      self.push_enum_instance(enum_, &scheme, &mono.type_args);
    }
  }

  fn push_enum_instance(
    &mut self,
    enum_: &ThirEnum,
    scheme: &TypeScheme,
    type_args: &SubstitutionMap,
  ) {
    let concrete_type_args = self.collect_type_args(scheme, type_args);
    let mangled_name = self.mangler.mangle_enum(enum_.def_id, &concrete_type_args);
    let variants = self.lower_enum_variants(enum_, type_args);

    let infer_args: Vec<InferTy> = scheme
      .vars
      .iter()
      .filter_map(|param| type_args.get(param.def_id).cloned())
      .collect();

    let ctx = self.builtin_ctx();
    let size = calculate_enum_size(&ctx, &enum_.def_id, &infer_args);
    let alignment = calculate_enum_alignment(&ctx, &enum_.def_id, &infer_args);
    let payload_layout = calculate_enum_payload_layout(&ctx, &enum_.def_id, &infer_args);

    self.ir.enums.push(IrEnum {
      name: mangled_name,
      id: IrId::new(),
      type_args: concrete_type_args,
      def_id: enum_.def_id,
      variants,
      layout: Layout::new(size, alignment.get()),
      payload_layout,
    });
  }

  fn lower_enum_variants(
    &self,
    enum_: &ThirEnum,
    type_args: &SubstitutionMap,
  ) -> Vec<IrEnumVariant> {
    let ctx = self.builtin_ctx();
    let discriminants = compute_variant_discriminants_from_exprs(
      &ctx,
      enum_.variants.iter().map(|variant| match &variant.kind {
        // TODO: fix
        // VariantKind::Discriminant(expr) => Some(expr),
        _ => None,
      }),
    );

    enum_
      .variants
      .iter()
      .enumerate()
      .map(|(idx, variant)| {
        let payload = match &variant.kind {
          VariantKind::Unit | VariantKind::Discriminant(_) => None,
          VariantKind::Tuple(tys) => {
            let types = tys
              .iter()
              .enumerate()
              .map(|(i, _)| {
                let field_name = get_or_intern(&i.to_string(), None);
                let infer_ty =
                  self.type_table.field_type(variant.def_id, field_name).unwrap_or_else(
                    || panic!("tuple variant field {i} should have a type"),
                  );
                let subst_ty = Self::apply_subst_by_def_id(&infer_ty, type_args);
                self.lower_type(&subst_ty)
              })
              .collect();
            Some(SemanticTy::Tuple(types))
          }
          VariantKind::Struct(fields) => {
            let types = fields
              .iter()
              .map(|field| {
                let infer_ty = self
                  .type_table
                  .field_type(variant.def_id, field.name)
                  .unwrap_or_else(|| {
                    panic!(
                      "struct variant field '{}' should have a type",
                      field.name.text()
                    )
                  });
                let subst_ty = Self::apply_subst_by_def_id(&infer_ty, type_args);
                self.lower_type(&subst_ty)
              })
              .collect();
            Some(SemanticTy::Tuple(types))
          }
        };

        IrEnumVariant {
          name: variant.name.text(),
          discriminant: discriminants[idx],
          payload,
        }
      })
      .collect()
  }

  fn push_struct_instance(
    &mut self,
    strukt: &ThirStruct,
    scheme: &TypeScheme,
    type_args: &SubstitutionMap,
  ) {
    let fields = self.lower_struct_fields(strukt, type_args);
    let concrete_type_args = self.collect_type_args(scheme, type_args);
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
    &self,
    scheme: &TypeScheme,
    type_args: &SubstitutionMap,
  ) -> Vec<SemanticTy> {
    scheme
      .vars
      .iter()
      .filter_map(|param| type_args.get(param.def_id).map(|ty| self.lower_type(ty)))
      .collect()
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
        (f.name.text(), self.lower_type(&substituted_ty))
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
        let struct_fields = self.thir.get_struct(def_id);

        for field in fields {
          let Some((field_idx, _)) =
            struct_fields.fields.iter().find_position(|f| f.name == field.name)
          else {
            unreachable!("checked before")
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

  pub fn lower_semantic_ty(
    &self,
    ty: &InferTy,
    type_args: &SubstitutionMap,
  ) -> SemanticTy {
    let substituted = Self::apply_subst_by_def_id(ty, type_args);
    self.lower_type(&substituted)
  }

  fn lower_type(&self, ty: &InferTy) -> SemanticTy {
    match ty {
      InferTy::Primitive(kind, _) => SemanticTy::Primitive(*kind),

      InferTy::Adt { def_id, args, .. } => {
        let args = args.iter().map(|a| self.lower_type(a)).collect();

        if self.thir.structs.iter().any(|s| s.def_id == *def_id) {
          return SemanticTy::Struct { def_id: *def_id, args };
        }

        if let Some(enum_entry) = self.thir.enums.get(def_id) {
          let ctx = self.builtin_ctx();
          let discriminants = compute_variant_discriminants_from_exprs(
            &ctx,
            enum_entry.variants.iter().map(|variant| match &variant.kind {
              // TODO: fix
              // VariantKind::Discriminant(expr) => Some(expr),
              _ => None,
            }),
          );

          let variants = enum_entry
            .variants
            .iter()
            .enumerate()
            .map(|(idx, v)| {
              let payload = match &v.kind {
                VariantKind::Unit => None,
                VariantKind::Tuple(tys) => Some(SemanticTy::Tuple(
                  tys.iter().map(|t| self.lower_type(t)).collect(),
                )),
                VariantKind::Struct(fields) => {
                  let tys = fields.iter().map(|f| self.lower_type(&f.ty)).collect();
                  Some(SemanticTy::Tuple(tys))
                }
                VariantKind::Discriminant(_) => None,
              };

              let discr = discriminants.get(idx).copied().unwrap_or(idx as u128) as i128;
              EnumVariant { name: v.name.text(), discr, payload }
            })
            .collect();

          return SemanticTy::Enum { def_id: *def_id, args, variants };
        }

        SemanticTy::Struct { def_id: *def_id, args }
      }

      InferTy::Ptr { mutability, ty: inner, .. } => SemanticTy::Ptr {
        mutability: *mutability,
        inner: Box::new(self.lower_type(inner)),
      },

      InferTy::Optional(inner, _) => {
        SemanticTy::Optional(Box::new(self.lower_type(inner)))
      }

      InferTy::Array { ty: inner, len, .. } => SemanticTy::Array {
        elem: Box::new(self.lower_type(inner)),
        len: len.expect_len(),
      },

      InferTy::Slice(inner, _) => SemanticTy::Slice(Box::new(self.lower_type(inner))),

      InferTy::Tuple(tys, _) => {
        SemanticTy::Tuple(tys.iter().map(|t| self.lower_type(t)).collect())
      }

      InferTy::Fn { params, ret, .. } => SemanticTy::Fn {
        params: params.iter().map(|p| self.lower_type(p)).collect(),
        ret: Box::new(self.lower_type(ret)),
      },

      InferTy::Unit(_) => SemanticTy::Unit,
      InferTy::Never(_) => SemanticTy::Never,

      InferTy::Var(_, _) | InferTy::Param(_) => SemanticTy::Error,
      InferTy::Err(_) => panic!("error shouldn't appear this late {ty:#?}"),
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
