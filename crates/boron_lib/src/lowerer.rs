use boron_context::BCtx;
use boron_session::prelude::Session;
use boron_source::ident_table::Identifier;
use boron_source::DefId;
use boron_source::DefIndex;
use boron_source::StableDefId;
use boron_types::hir::PatKind;
use std::cell::RefCell;
use std::collections::HashMap;

use boron_types::thir::{
  Block, Enum, EnumVariantStructField, Expr, ExprKind, Field, FieldInit, Function,
  GenericParam, GenericParamKind, Generics, Local, MatchArm, Param, Stmt, StmtKind,
  Struct, Thir, TypeBound, Variant, VariantKind,
};

use boron_session::library::{
  BlibBlock, BlibBody, BlibEnum, BlibEnumField, BlibExpr, BlibExprKind, BlibField,
  BlibFieldInit, BlibFunction, BlibGenericParam, BlibGenericParamKind, BlibGenerics,
  BlibItem, BlibLiteral, BlibLocal, BlibMatchArm, BlibParam, BlibStmt, BlibStruct,
  BlibTy, BlibTyParam, BlibTypeBound, BlibVariant, BlibVariantKind,
};
use boron_types::infer_ty::InferTy;
use boron_types::literal_table::FullLiteral;

pub struct ThirToBlibLowerer<'ctx> {
  sess: &'ctx Session,
  ctx: &'ctx BCtx<'ctx>,
  generic_param_indices: RefCell<HashMap<DefId, u32>>,
}

impl<'ctx> ThirToBlibLowerer<'ctx> {
  pub fn new(sess: &'ctx Session, ctx: &'ctx BCtx<'ctx>) -> Self {
    Self { sess, ctx, generic_param_indices: RefCell::new(HashMap::new()) }
  }

  pub fn lower_function(&self, f: &Function) -> BlibFunction {
    self.with_generics(&f.generics, || {
      let generics = self.lower_generics(&f.generics);
      let params = f.params.iter().map(|p| self.lower_param(p)).collect();
      let return_type = self.lower_ty(&f.return_type);

      let mut func = BlibFunction {
        id: self.sid(f.def_id),
        name: f.name,
        generics,
        params,
        return_type,
        body: None,
      };

      if let Some(body) = &f.body {
        let blib_body = BlibBody { block: self.lower_block(body) };
        func.set_body_if_generic(blib_body);
      }

      func
    })
  }

  pub fn lower_struct(&self, s: &Struct) -> BlibStruct {
    self.with_generics(&s.generics, || BlibStruct {
      id: self.sid(s.def_id),
      name: s.name,
      generics: self.lower_generics(&s.generics),
      fields: s.fields.iter().map(|f| self.lower_field(f)).collect(),
    })
  }

  pub fn lower_enum(&self, e: &Enum) -> BlibEnum {
    self.with_generics(&e.generics, || BlibEnum {
      id: self.sid(e.def_id),
      name: e.name,
      generics: self.lower_generics(&e.generics),
      variants: e.variants.iter().map(|v| self.lower_variant(v)).collect(),
    })
  }

  fn with_generics<R>(&self, generics: &Generics, f: impl FnOnce() -> R) -> R {
    let previous = self.generic_param_indices.replace(
      generics
        .params
        .iter()
        .enumerate()
        .map(|(index, param)| (param.def_id, index as u32))
        .collect(),
    );
    let result = f();
    self.generic_param_indices.replace(previous);
    result
  }

  fn lower_generics(&self, g: &Generics) -> BlibGenerics {
    BlibGenerics {
      params: g.params.iter().map(|p| self.lower_generic_param(p)).collect(),
    }
  }

  fn lower_generic_param(&self, p: &GenericParam) -> BlibGenericParam {
    BlibGenericParam {
      id: self.sid(p.def_id),
      name: p.name,
      kind: match &p.kind {
        GenericParamKind::Type { bounds } => BlibGenericParamKind::Type {
          bounds: bounds.iter().map(|b| self.lower_type_bound(b)).collect(),
        },
        GenericParamKind::Const { ty } => {
          BlibGenericParamKind::Const { ty: self.lower_ty(ty) }
        }
      },
    }
  }

  fn lower_type_bound(&self, b: &TypeBound) -> BlibTypeBound {
    BlibTypeBound { id: self.sid(b.def_id) }
  }

  fn lower_ty(&self, ty: &InferTy) -> BlibTy {
    match ty {
      InferTy::Primitive(p, _) => BlibTy::Primitive(*p),
      InferTy::Adt { def_id, args, .. } => BlibTy::Adt {
        id: self.sid(*def_id),
        args: args.iter().map(|a| self.lower_ty(a)).collect(),
      },
      InferTy::Ptr { mutability, ty, .. } => {
        BlibTy::Ptr { mutability: *mutability, ty: Box::new(self.lower_ty(ty)) }
      }
      InferTy::Optional(inner, _) => BlibTy::Optional(Box::new(self.lower_ty(inner))),
      InferTy::Array { ty, len, .. } => {
        BlibTy::Array { ty: Box::new(self.lower_ty(ty)), len: len.len() }
      }
      InferTy::Slice(inner, _) => BlibTy::Slice(Box::new(self.lower_ty(inner))),
      InferTy::Tuple(elems, _) => {
        BlibTy::Tuple(elems.iter().map(|e| self.lower_ty(e)).collect())
      }
      InferTy::Fn { params, ret, .. } => BlibTy::Fn {
        params: params.iter().map(|p| self.lower_ty(p)).collect(),
        ret: Box::new(self.lower_ty(ret)),
      },
      InferTy::Unit(_) => BlibTy::Unit,
      InferTy::Never(_) => BlibTy::Never,
      InferTy::Param(p) => BlibTy::Param(BlibTyParam {
        name: p.name,
        index: *self
          .generic_param_indices
          .borrow()
          .get(&p.def_id)
          .expect("generic parameter must exist in declaration order map"),
      }),
      InferTy::Var(_, span) => {
        panic!("unresolved inference variable at {span:?} during blib lowering")
      }
      InferTy::Err(_) => BlibTy::Never,
    }
  }

  fn lower_param(&self, p: &Param) -> BlibParam {
    BlibParam { name: p.name, id: self.sid(p.def_id), ty: self.lower_ty(&p.ty) }
  }

  fn lower_field(&self, f: &Field) -> BlibField {
    BlibField { name: f.name, ty: self.lower_ty(&f.ty) }
  }

  fn lower_variant(&self, v: &Variant) -> BlibVariant {
    BlibVariant {
      id: self.sid(v.def_id),
      name: v.name,
      kind: self.lower_variant_kind(&v.kind),
    }
  }

  fn lower_variant_kind(&self, kind: &VariantKind) -> BlibVariantKind {
    match kind {
      VariantKind::Unit => BlibVariantKind::Unit,
      VariantKind::Tuple(tys) => {
        BlibVariantKind::Tuple(tys.iter().map(|t| self.lower_ty(t)).collect())
      }
      VariantKind::Struct(fields) => {
        BlibVariantKind::Struct(fields.iter().map(|f| self.lower_enum_field(f)).collect())
      }
      VariantKind::Discriminant(_) => BlibVariantKind::Discriminant(),
    }
  }

  fn lower_enum_field(&self, f: &EnumVariantStructField) -> BlibEnumField {
    BlibEnumField { name: f.name, ty: self.lower_ty(&f.ty) }
  }

  fn lower_block(&self, block: &Block) -> BlibBlock {
    BlibBlock {
      stmts: block.stmts.iter().map(|s| self.lower_stmt(s)).collect(),
      expr: block.expr.as_ref().map(|e| Box::new(self.lower_expr(e))),
    }
  }

  fn lower_stmt(&self, stmt: &Stmt) -> BlibStmt {
    match &stmt.kind {
      StmtKind::Local(local) => BlibStmt::Local(self.lower_local(local)),
      StmtKind::Expr(expr) => BlibStmt::Expr(self.lower_expr(expr)),
    }
  }

  fn lower_local(&self, local: &Local) -> BlibLocal {
    let (name, id) = match &local.pat.kind {
      PatKind::Binding { def_id, name, .. } => (*name, self.sid(*def_id)),
      _ => (Identifier::dummy(), self.anon_sid(local)),
    };

    BlibLocal {
      id,
      name,
      ty: self.lower_ty(&local.ty),
      init: local.init.as_ref().map(|e| self.lower_expr(e)),
    }
  }

  fn lower_expr(&self, expr: &Expr) -> BlibExpr {
    BlibExpr { ty: self.lower_ty(&expr.ty), kind: self.lower_expr_kind(&expr.kind) }
  }

  fn lower_expr_kind(&self, kind: &ExprKind) -> BlibExprKind {
    match kind {
      ExprKind::Literal(lit) => BlibExprKind::Literal(self.lower_literal(lit)),
      ExprKind::LocalRef(id) => BlibExprKind::LocalRef(self.sid(*id)),
      ExprKind::Path(id) => BlibExprKind::Path(self.sid(*id)),
      ExprKind::Binary { op, lhs, rhs } => BlibExprKind::Binary {
        op: *op,
        lhs: Box::new(self.lower_expr(lhs)),
        rhs: Box::new(self.lower_expr(rhs)),
      },
      ExprKind::Unary { op, operand } => {
        BlibExprKind::Unary { op: *op, operand: Box::new(self.lower_expr(operand)) }
      }
      ExprKind::Assign { target, value } => BlibExprKind::Assign {
        target: Box::new(self.lower_expr(target)),
        value: Box::new(self.lower_expr(value)),
      },
      ExprKind::Cast { expr, ty } => BlibExprKind::Cast {
        expr: Box::new(self.lower_expr(expr)),
        ty: self.lower_ty(ty),
      },
      ExprKind::Call { callee, callee_name, type_args, args } => BlibExprKind::Call {
        callee: self.sid(*callee),
        callee_name: *callee_name,
        type_args: type_args.iter().map(|t| self.lower_ty(t)).collect(),
        args: args.iter().map(|a| self.lower_expr(a)).collect(),
      },
      ExprKind::Field { object, field } => {
        BlibExprKind::Field { object: Box::new(self.lower_expr(object)), field: *field }
      }
      ExprKind::Index { object, index } => BlibExprKind::Index {
        object: Box::new(self.lower_expr(object)),
        index: Box::new(self.lower_expr(index)),
      },
      ExprKind::Struct { def_id, type_args, fields } => BlibExprKind::Struct {
        id: self.sid(*def_id),
        type_args: type_args.iter().map(|t| self.lower_ty(t)).collect(),
        fields: fields.iter().map(|f| self.lower_field_init(f)).collect(),
      },
      ExprKind::Tuple(elems) => {
        BlibExprKind::Tuple(elems.iter().map(|e| self.lower_expr(e)).collect())
      }
      ExprKind::Array(elems) => {
        BlibExprKind::Array(elems.iter().map(|e| self.lower_expr(e)).collect())
      }
      ExprKind::Block(block) => BlibExprKind::Block(self.lower_block(block)),
      ExprKind::If { condition, then_block, else_branch } => BlibExprKind::If {
        condition: Box::new(self.lower_expr(condition)),
        then_block: self.lower_block(then_block),
        else_branch: else_branch.as_ref().map(|e| Box::new(self.lower_expr(e))),
      },
      ExprKind::Match { scrutinee, arms } => BlibExprKind::Match {
        scrutinee: Box::new(self.lower_expr(scrutinee)),
        arms: arms.iter().map(|a| self.lower_match_arm(a)).collect(),
      },
      ExprKind::Loop { body } => BlibExprKind::Loop { body: self.lower_block(body) },
      ExprKind::Break { value } => BlibExprKind::Break {
        value: value.as_ref().map(|v| Box::new(self.lower_expr(v))),
      },
      ExprKind::Continue => BlibExprKind::Continue,
      ExprKind::Return { value } => BlibExprKind::Return {
        value: value.as_ref().map(|v| Box::new(self.lower_expr(v))),
      },
      ExprKind::Err => BlibExprKind::Err,
    }
  }

  fn lower_field_init(&self, f: &FieldInit) -> BlibFieldInit {
    BlibFieldInit {
      name: f.name,
      ty: self.lower_ty(&f.ty),
      value: self.lower_expr(&f.value),
    }
  }

  fn lower_match_arm(&self, arm: &MatchArm) -> BlibMatchArm {
    BlibMatchArm { body: self.lower_expr(&arm.body) }
  }

  fn lower_literal(&self, lit: &FullLiteral) -> BlibLiteral {
    match lit {
      FullLiteral::Int(v) => BlibLiteral::Int(v.to_string()),
      FullLiteral::Float(v) => BlibLiteral::Float(v.to_string()),
      FullLiteral::Bool(b) => BlibLiteral::Bool(*b),
      FullLiteral::Char(c) => BlibLiteral::Char(*c),
      FullLiteral::String(s) => BlibLiteral::String(s.clone()),
      FullLiteral::Unit => BlibLiteral::Unit,
    }
  }

  #[inline]
  fn sid(&self, id: DefId) -> StableDefId {
    self.ctx.stable_def_id(id)
  }

  #[inline]
  fn anon_sid(&self, local: &Local) -> StableDefId {
    let synthetic = DefId {
      def_index: DefIndex(local.hir_id.index() as usize),
      package_index: self.ctx.current_pkg_id(),
    };

    self.sid(synthetic)
  }
}

pub fn lower_thir_to_blib<'ctx>(
  sess: &'ctx Session,
  ctx: &'ctx BCtx<'ctx>,
) -> Vec<BlibItem> {
  let _ = ctx.current_pkg_id();

  let lowerer = ThirToBlibLowerer::new(sess, ctx);
  let thir: &Thir = ctx.thir();
  let mut items = Vec::new();

  for entry in thir.functions.iter() {
    if ctx.is_local(*entry.key()) {
      items.push(BlibItem::Function(lowerer.lower_function(entry.value())));
    }
  }
  for entry in thir.structs.iter() {
    if ctx.is_local(*entry.key()) {
      items.push(BlibItem::Struct(lowerer.lower_struct(entry.value())));
    }
  }
  for entry in thir.enums.iter() {
    if ctx.is_local(*entry.key()) {
      items.push(BlibItem::Enum(lowerer.lower_enum(entry.value())));
    }
  }

  items
}
