use crate::errors::ArrayLenNotANumber;
use crate::interpreter::values::ConstValue;
use crate::interpreter::{InterpreterContext, InterpreterMode};
use crate::ty::TyParam;
use crate::{InferTy, TyChecker, TypeEnv};
use boron_hir::ty::ArrayLen;
use boron_hir::{GenericParamKind, Generics, Ty, TyKind};
use boron_resolver::DefKind;
use boron_source::prelude::Span;
use boron_utils::prelude::Identifier;

impl TyChecker<'_> {
  pub fn lower_hir_ty(&self, ty: &Ty) -> InferTy {
    match &ty.kind {
      TyKind::Infer => self.infcx.fresh(ty.span),
      TyKind::Primitive(p) => InferTy::Primitive(*p, ty.span),
      TyKind::Path { def_id, segments } => {
        if let Some(def) = self.resolver.get_definition(*def_id)
          && def.kind == DefKind::TypeParam
        {
          return InferTy::Param(TyParam {
            def_id: *def_id,
            span: Span::dummy(), // todo: span
            name: Identifier::new(&def.name, Span::dummy())
          })
        }
        self.check_path(*def_id, &TypeEnv::new());

        let infer_args = segments
          .iter()
          .flat_map(|seg| seg.args.iter())
          .map(|t| self.lower_hir_ty(t))
          .collect();

        InferTy::Adt { def_id: *def_id, args: infer_args, span: ty.span }
      }

      TyKind::Ptr { mutability, ty: inner } => InferTy::Ptr {
        mutability: *mutability,
        ty: Box::new(self.lower_hir_ty(inner)),
        span: ty.span,
      },
      TyKind::Optional(inner) => {
        InferTy::Optional(Box::new(self.lower_hir_ty(inner)), ty.span)
      }
      TyKind::Array { ty: inner, len } => {
        let array_len = match len {
          ArrayLen::Const(n) => *n,
          ArrayLen::ConstExpr(expr) => {
            let value = self
              .new_interpreter(InterpreterMode::Const, InterpreterContext::ArrayLen)
              .evaluate_expr(expr);

            match value {
              ConstValue::Int(i) => i as usize,
              ConstValue::Poison => 0,
              _ => {
                self
                  .dcx()
                  .emit(ArrayLenNotANumber { found: value.to_string(), span: expr.span });
                0
              }
            }
          }
        };
        InferTy::Array {
          ty: Box::new(self.lower_hir_ty(inner)),
          len: array_len,
          span: ty.span,
        }
      }
      TyKind::Slice(inner) => InferTy::Slice(Box::new(self.lower_hir_ty(inner)), ty.span),
      TyKind::Tuple(tys) => {
        InferTy::Tuple(tys.iter().map(|t| self.lower_hir_ty(t)).collect(), ty.span)
      }
      TyKind::Unit => InferTy::Unit(ty.span),
      TyKind::Never => InferTy::Never(ty.span),
      TyKind::Err => InferTy::Err(ty.span),
      TyKind::Fn { params, ret } => InferTy::Fn {
        params: params.iter().map(|t| self.lower_hir_ty(t)).collect(),
        ret: Box::new(self.lower_hir_ty(ret)),
        span: ty.span,
      },
    }
  }

  pub fn register_generics(&self, generics: &Generics) -> Vec<TyParam> {
    self.infcx.clear_type_params();

    let mut ty_params = Vec::new();
    for param in &generics.params {
      if matches!(param.kind, GenericParamKind::Type { .. }) {
        ty_params.push(TyParam {
          name: param.name,
          def_id: param.def_id,
          span: param.span,
        });
      }
    }

    ty_params
  }
}
