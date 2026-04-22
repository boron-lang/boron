use boron_resolver::DefId;
use boron_session::prelude::Span;
use boron_types::infer_ty::{InferTy, TyVar, TyVarKind};
use dashmap::DashMap;
use std::collections::{HashMap, HashSet};

#[derive(Debug)]
pub struct InferCtx {
  pub(crate) var_kinds: DashMap<TyVar, TyVarKind>,
  /// maps type variables to their resolved types
  pub(crate) substitution: DashMap<TyVar, InferTy>,
  /// Maps type parameter `DefIds` to their type variables
  pub type_params: DashMap<DefId, TyVar>,
}

impl InferCtx {
  pub fn new() -> Self {
    Self {
      var_kinds: DashMap::new(),
      substitution: DashMap::new(),
      type_params: DashMap::new(),
    }
  }

  pub fn clear_type_params(&self) {
    self.type_params.clear();
  }

  pub fn get_or_create_type_param(&self, def_id: DefId) -> TyVar {
    if let Some(var) = self.type_params.get(&def_id) {
      return *var.value();
    }

    let var = self.fresh_var(TyVarKind::General);
    self.type_params.insert(def_id, var);
    var
  }

  pub fn lookup_type_param(&self, def_id: DefId) -> Option<TyVar> {
    self.type_params.get(&def_id).map(|w| *w.value())
  }

  pub fn fresh_var(&self, kind: TyVarKind) -> TyVar {
    let var = TyVar::new();
    self.var_kinds.insert(var, kind);
    var
  }

  pub fn fresh(&self, span: Span) -> InferTy {
    InferTy::Var(self.fresh_var(TyVarKind::General), span)
  }

  pub fn fresh_int(&self, span: Span) -> InferTy {
    InferTy::Var(self.fresh_var(TyVarKind::Integer), span)
  }

  pub fn fresh_float(&self, span: Span) -> InferTy {
    InferTy::Var(self.fresh_var(TyVarKind::Float), span)
  }

  pub fn var_kind(&self, var: TyVar) -> TyVarKind {
    self.var_kinds.get(&var).map(|k| *k).unwrap_or(TyVarKind::General)
  }

  pub fn unify_var(&self, var: TyVar, ty: InferTy) {
    if let InferTy::Var(v, _) = ty
      && v == var
    {
      return;
    }

    self.substitution.insert(var, ty);
  }

  pub fn probe_var(&self, var: TyVar) -> Option<InferTy> {
    self.substitution.get(&var).map(|t| t.clone())
  }

  pub fn resolve(&self, ty: &InferTy) -> InferTy {
    self.resolve_with_seen(ty, &mut HashSet::new())
  }

  fn resolve_with_seen(&self, ty: &InferTy, seen: &mut HashSet<TyVar>) -> InferTy {
    match ty {
      InferTy::Var(var, _) => {
        if seen.contains(var) {
          return ty.clone();
        }

        if let Some(resolved) = self.probe_var(*var) {
          seen.insert(*var);
          let result = self.resolve_with_seen(&resolved, seen);
          seen.remove(var);
          result
        } else {
          ty.clone()
        }
      }
      InferTy::Adt { def_id, args, span } => InferTy::Adt {
        def_id: *def_id,
        args: args.iter().map(|t| self.resolve_with_seen(t, seen)).collect(),
        span: *span,
      },
      InferTy::Ptr { mutability, ty, span } => InferTy::Ptr {
        mutability: *mutability,
        ty: Box::new(self.resolve_with_seen(ty, seen)),
        span: *span,
      },
      InferTy::Optional(ty, span) => {
        InferTy::Optional(Box::new(self.resolve_with_seen(ty, seen)), *span)
      }
      InferTy::Array { ty, len, span } => InferTy::Array {
        ty: Box::new(self.resolve_with_seen(ty, seen)),
        len: *len,
        span: *span,
      },
      InferTy::Slice(ty, span) => {
        InferTy::Slice(Box::new(self.resolve_with_seen(ty, seen)), *span)
      }
      InferTy::Tuple(tys, span) => InferTy::Tuple(
        tys.iter().map(|t| self.resolve_with_seen(t, seen)).collect(),
        *span,
      ),
      InferTy::Fn { params, ret, span } => InferTy::Fn {
        params: params.iter().map(|t| self.resolve_with_seen(t, seen)).collect(),
        ret: Box::new(self.resolve_with_seen(ret, seen)),
        span: *span,
      },
      _ => ty.clone(),
    }
  }
}

impl Default for InferCtx {
  fn default() -> Self {
    Self::new()
  }
}

/// Tracks the types of local variables during type checking of a function body.
#[derive(Debug, Clone)]
pub struct TypeEnv {
  scopes: Vec<HashMap<DefId, InferTy>>,
}

impl TypeEnv {
  pub fn new() -> Self {
    Self { scopes: vec![HashMap::new()] }
  }

  pub fn push_scope(&mut self) {
    self.scopes.push(HashMap::new());
  }

  pub fn pop_scope(&mut self) {
    self.scopes.pop();
  }

  pub fn bind(&mut self, def_id: DefId, ty: InferTy) {
    if let Some(scope) = self.scopes.last_mut() {
      scope.insert(def_id, ty);
    }
  }

  pub fn lookup(&self, def_id: DefId) -> Option<&InferTy> {
    for scope in self.scopes.iter().rev() {
      if let Some(ty) = scope.get(&def_id) {
        return Some(ty);
      }
    }
    None
  }
}

impl Default for TypeEnv {
  fn default() -> Self {
    Self::new()
  }
}
