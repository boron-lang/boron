use crate::{
  Ir, IrBlock, IrBody, IrExpr, IrExprKind, IrFieldInit, IrFunction, IrId, IrLocal,
  IrParam, IrStmt, IrStmtKind, IrStruct, Projection, SymbolMangler,
};
use boron_analysis::ty::SubstitutionMap;
use boron_analysis::{InferTy, TypeScheme, TypeTable};
use boron_hir::pat::PatKind;
use boron_hir::{Hir, ParamKind, Pat, SemanticTy};
use boron_session::prelude::debug;
use boron_thir::{
  Block as ThirBlock, Expr as ThirExpr, ExprKind as ThirExprKind,
  FieldInit as ThirFieldInit, Function as ThirFunction, Struct as ThirStruct, Thir,
};
use itertools::Itertools as _;

#[derive(Debug)]
struct LoweringContext {
  #[expect(dead_code)]
  owner: boron_resolver::DefId,
  type_args: SubstitutionMap,
}

#[derive(Debug)]
pub struct IrLowerer<'a> {
  hir: &'a Hir,
  thir: &'a Thir,
  type_table: &'a TypeTable,
  pub ir: Ir,
  mangler: SymbolMangler<'a>,
  pub current_function: IrId,
  lowering_context: Option<LoweringContext>,
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
      lowering_context: None,
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

  // ── Body / block / expr lowering ──────────────────────────────────

  pub(crate) fn lower_body(
    &mut self,
    block: &ThirBlock,
    type_args: &SubstitutionMap,
    owner: boron_resolver::DefId,
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
        boron_thir::StmtKind::Local(local) => {
          let type_args = &self.context().type_args;
          let ty = Self::lower_semantic_ty(&local.ty, type_args);
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
        boron_thir::StmtKind::Expr(expr) => {
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
    let ty = Self::lower_semantic_ty(&expr.ty, type_args);

    let kind = match &expr.kind {
      ThirExprKind::Literal(lit) => IrExprKind::Literal(lit.clone()),
      ThirExprKind::LocalRef(def_id) => IrExprKind::LocalRef(*def_id),
      ThirExprKind::Path(def_id) => IrExprKind::GlobalRef(*def_id),

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
        let cast_ty = Self::lower_semantic_ty(ty, &self.context().type_args);
        IrExprKind::Cast { expr: Box::new(inner), ty: cast_ty }
      }
      ThirExprKind::Call { callee, type_args: call_type_args, args } => {
        let args = args.iter().map(|arg| self.lower_expr(arg)).collect();
        let type_args = call_type_args
          .iter()
          .map(|ty| Self::lower_semantic_ty(ty, &self.context().type_args))
          .collect();
        IrExprKind::Call { callee: *callee, type_args, args }
      }
      ThirExprKind::Field { object, field } => {
        let object = self.lower_expr(object);
        IrExprKind::Field { object: Box::new(object), field: *field }
      }
      ThirExprKind::Index { object, index } => {
        let object = self.lower_expr(object);
        let index = self.lower_expr(index);
        IrExprKind::Index { object: Box::new(object), index: Box::new(index) }
      }
      ThirExprKind::AddrOf { operand } => {
        let operand = self.lower_expr(operand);
        IrExprKind::AddrOf { operand: Box::new(operand) }
      }
      ThirExprKind::Struct { def_id, type_args: struct_type_args, fields } => {
        let fields = fields.iter().map(|f| self.lower_field_init(f)).collect();
        let type_args = struct_type_args
          .iter()
          .map(|ty| Self::lower_semantic_ty(ty, &self.context().type_args))
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
        panic!("err expr shouldn't exist at this point: {:#?}", expr)
      }
    };

    IrExpr { hir_id: expr.hir_id, ty, kind, span: expr.span }
  }

  fn lower_field_init(&mut self, field: &ThirFieldInit) -> IrFieldInit {
    let type_args = &self.context().type_args;
    let ty = Self::lower_semantic_ty(&field.ty, type_args);

    IrFieldInit {
      hir_id: field.hir_id,
      name: field.name.text(),
      ty,
      value: self.lower_expr(&field.value),
      span: field.span,
    }
  }

  // ── Struct lowering ───────────────────────────────────────────────

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

  // ── Type lowering ─────────────────────────────────────────────────

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
