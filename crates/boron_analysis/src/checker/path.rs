use crate::checker::TyChecker;
use crate::interpreter::InterpreterContext;
use crate::table::TypeEnv;
use boron_resolver::{DefId, DefKind};
use boron_types::ast::InterpreterMode;
use boron_types::infer_ty::{InferTy, SubstitutionMap, TypeScheme};

impl<'a> TyChecker<'a> {
  pub(crate) fn check_path(
    &self,
    id: DefId,
    env: &TypeEnv,
    explicit_args: Option<&[InferTy]>,
  ) -> InferTy {
    if let Some(cnst) = self.ctx.hir_const(id) {
      let _ = self
        .new_interpreter(InterpreterMode::Const, InterpreterContext::Const)
        .evaluate_const(&cnst);
    }

    if let Some(ty) = env.lookup(id) {
      return ty.clone();
    }
    let id = if let Some(self_id) = self.ctx.self_mapping(id) {
      self_id
    } else if let Some(parent) = self.ctx.adt_parent(id)
      && let Some(def) = self.ctx.get_definition(id)
      && matches!(def.kind, DefKind::Variant)
    {
      parent
    } else {
      id
    };

    if let Some(scheme) = self.ctx.def_type(id) {
      if let Some(args) = explicit_args {
        if !args.is_empty() && args.len() == scheme.vars.len() {
          return Self::instantiate_with_args(&scheme, args).0;
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

    (scheme.ty.apply_subst(&subst), subst)
  }

  pub fn substitute_with_scheme(
    ty: &InferTy,
    args: Vec<InferTy>,
    scheme: TypeScheme,
  ) -> InferTy {
    scheme
      .substitution_for_args(args.as_slice())
      .map(|subst| Self::apply_subst(ty, &subst))
      .unwrap_or_else(|| ty.clone())
  }

  pub fn instantiate_with_args(
    scheme: &TypeScheme,
    args: &[InferTy],
  ) -> (InferTy, SubstitutionMap) {
    let subst = scheme.substitution_for_args(args).unwrap_or_default();
    (Self::apply_subst(&scheme.ty, &subst), subst)
  }

  pub(crate) fn apply_subst(ty: &InferTy, subst: &SubstitutionMap) -> InferTy {
    ty.apply_subst(subst)
  }
}
