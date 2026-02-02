use crate::checker::TyChecker;
use crate::interpreter::InterpreterContext;
use crate::interpreter::InterpreterMode;
use crate::table::TypeEnv;
use crate::ty::{InferTy, SubstitutionMap, TypeScheme};
use boron_resolver::DefId;

impl TyChecker<'_> {
  pub(crate) fn check_path(
    &self,
    id: DefId,
    env: &TypeEnv,
    explicit_args: Option<&[InferTy]>,
  ) -> InferTy {
    if let Some(cnst) = self.hir.get_const(id) {
      let _ = self
        .new_interpreter(InterpreterMode::Const, InterpreterContext::Const)
        .evaluate_const(&cnst);
    }

    if let Some(ty) = env.lookup(id) {
      return ty.clone();
    }

    if let Some(scheme) = self.table.def_type(id) {
      if let Some(args) = explicit_args {
        if !args.is_empty() && args.len() == scheme.vars.len() {
          return self.instantiate_with_args(&scheme, args).0;
        }
      }
      return self.instantiate(&scheme).0;
    }

    InferTy::Err(Default::default())
  }

  pub(crate) fn instantiate(&self, scheme: &TypeScheme) -> (InferTy, SubstitutionMap) {
    if scheme.vars.is_empty() {
      return (scheme.ty.clone(), SubstitutionMap::new());
    }

    let mut subst = SubstitutionMap::new();
    for &var in &scheme.vars {
      subst.add(var, self.infcx.fresh(scheme.ty.span()));
    }

    (Self::apply_subst(&scheme.ty, &subst), subst)
  }

  pub(crate) fn instantiate_with_args(
    &self,
    scheme: &TypeScheme,
    args: &[InferTy],
  ) -> (InferTy, SubstitutionMap) {
    if scheme.vars.is_empty() {
      return (scheme.ty.clone(), SubstitutionMap::new());
    }

    let mut subst = SubstitutionMap::new();
    for (var, arg) in scheme.vars.iter().zip(args.iter()) {
      subst.add(*var, arg.clone());
    }

    (Self::apply_subst(&scheme.ty, &subst), subst)
  }

  pub(crate) fn apply_subst(ty: &InferTy, subst: &SubstitutionMap) -> InferTy {
    match ty {
      InferTy::Var(_var, _span) => ty.clone(),
      InferTy::Param(p) => {
        if let Some(replacement) = subst.get(p.def_id) {
          replacement.clone()
        } else {
          ty.clone()
        }
      }
      InferTy::Adt { def_id, args, span } => InferTy::Adt {
        def_id: *def_id,
        args: args.iter().map(|t| Self::apply_subst(t, subst)).collect(),
        span: *span,
      },
      InferTy::Ptr { mutability, ty: inner, span } => InferTy::Ptr {
        mutability: *mutability,
        ty: Box::new(Self::apply_subst(inner, subst)),
        span: *span,
      },
      InferTy::Optional(inner, span) => {
        InferTy::Optional(Box::new(Self::apply_subst(inner, subst)), *span)
      }
      InferTy::Array { ty: inner, len, span } => InferTy::Array {
        ty: Box::new(Self::apply_subst(inner, subst)),
        len: *len,
        span: *span,
      },
      InferTy::Slice(inner, span) => {
        InferTy::Slice(Box::new(Self::apply_subst(inner, subst)), *span)
      }
      InferTy::Tuple(tys, span) => {
        InferTy::Tuple(tys.iter().map(|t| Self::apply_subst(t, subst)).collect(), *span)
      }
      InferTy::Fn { params, ret, span } => InferTy::Fn {
        params: params.iter().map(|t| Self::apply_subst(t, subst)).collect(),
        ret: Box::new(Self::apply_subst(ret, subst)),
        span: *span,
      },
      _ => ty.clone(),
    }
  }
}
