use crate::checker::TyChecker;
use crate::errors::{NotAllFieldsCovered, RefutablePatternInLocalBinding};
use crate::table::TypeEnv;
use crate::ty::{ArrayLength, InferTy, SubstitutionMap};
use crate::unify::Expectation;
use boron_hir::{Literal, Pat, PatKind};
use boron_resolver::{DefId, DefKind};
use boron_source::span::Span;
use itertools::Itertools as _;

#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub enum ExpectedPattern {
  Refutable,
  Irrefutable,
}

#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub enum PatternContext {
  Local,
}

#[derive(Debug, Clone, PartialEq, Eq, Copy)]
enum PatternRefutability {
  Refutable,
  Irrefutable,
}

impl TyChecker<'_> {
  pub(crate) fn check_pattern(
    &mut self,
    pat: &Pat,
    expected: &InferTy,
    env: &mut TypeEnv,
    expected_pat: ExpectedPattern,
    ctx: PatternContext,
  ) {
    match expected_pat {
      ExpectedPattern::Irrefutable => {
        self.check_irrefutable_pattern(pat, expected, env, ctx);
      }
      ExpectedPattern::Refutable => self.check_refutable_pattern(pat, expected, env, ctx),
    }
  }

  fn check_irrefutable_pattern(
    &mut self,
    pat: &Pat,
    expected: &InferTy,
    env: &mut TypeEnv,
    _ctx: PatternContext,
  ) {
    let refutability = self.check_pattern_internal(pat, expected, env, _ctx, true);

    if refutability == PatternRefutability::Refutable {
      self.dcx().emit(RefutablePatternInLocalBinding { span: pat.span });
    }
  }

  fn check_refutable_pattern(
    &mut self,
    pat: &Pat,
    expected: &InferTy,
    env: &mut TypeEnv,
    _ctx: PatternContext,
  ) {
    self.check_pattern_internal(pat, expected, env, _ctx, false);
  }

  fn check_pattern_internal(
    &mut self,
    pat: &Pat,
    expected: &InferTy,
    env: &mut TypeEnv,
    _ctx: PatternContext,
    require_full_struct: bool,
  ) -> PatternRefutability {
    self.table.record_node_type(pat.hir_id, expected.clone());

    match &pat.kind {
      PatKind::Wild => PatternRefutability::Irrefutable,
      PatKind::Err => PatternRefutability::Irrefutable,

      PatKind::Binding { def_id, subpat, .. } => {
        env.bind(*def_id, expected.clone());

        if let Some(subpat) = subpat {
          self.check_pattern_internal(subpat, expected, env, _ctx, require_full_struct)
        } else {
          PatternRefutability::Irrefutable
        }
      }

      PatKind::Literal(lit) => {
        let lit_ty = self.check_literal_with_span(lit, pat.span);
        let result = self.unify(&lit_ty, expected);
        self.handle_unify_result(result, pat.span);

        if matches!(lit, Literal::Unit) {
          PatternRefutability::Irrefutable
        } else {
          PatternRefutability::Refutable
        }
      }

      PatKind::Tuple(pats) => {
        let elem_tys = self.tuple_expected_types(pats.len(), expected, pat.span);
        let mut refutable = PatternRefutability::Irrefutable;

        for (elem_pat, elem_ty) in pats.iter().zip(elem_tys.iter()) {
          if self.check_pattern_internal(
            elem_pat,
            elem_ty,
            env,
            _ctx,
            require_full_struct,
          ) == PatternRefutability::Refutable
          {
            refutable = PatternRefutability::Refutable;
          }
        }

        refutable
      }

      PatKind::Struct { rest, fields, def_id } => {
        let subst = self.struct_subst_from_expected(*def_id, expected, pat.span);
        let strukt = self.hir.get_struct(*def_id).expect("should exist");
        let mut missing_fields = vec![];

        for field in &strukt.fields {
          if !fields.iter().any(|f| f.name == field.name) {
            missing_fields.push(field);
          }
        }

        if require_full_struct && !rest && !missing_fields.is_empty() {
          self.dcx().emit(NotAllFieldsCovered {
            span: pat.span,
            fields: missing_fields.iter().map(|f| f.name.text()).collect_vec().join(", "),
          });
        }

        let mut refutable = PatternRefutability::Irrefutable;

        for field in fields {
          let base_field_ty =
            strukt.fields.iter().find(|f| f.name == field.name).unwrap().clone().ty;
          let lowered_ty = &self.lower_hir_ty(&base_field_ty);
          let field_ty = Self::apply_subst(lowered_ty, &subst);

          if self.check_pattern_internal(
            &field.pat,
            &field_ty,
            env,
            _ctx,
            require_full_struct,
          ) == PatternRefutability::Refutable
          {
            refutable = PatternRefutability::Refutable;
          }
        }

        refutable
      }

      PatKind::TupleStruct { def_id, pats } => {
        let is_struct = self.def_kind_is_struct(*def_id);
        if is_struct {
          self.ensure_struct_type(*def_id, expected, pat.span);
        }

        let elem_tys: Vec<InferTy> =
          (0..pats.len()).map(|_| self.infcx.fresh(pat.span)).collect();
        let mut refutable = PatternRefutability::Irrefutable;

        for (elem_pat, elem_ty) in pats.iter().zip(elem_tys.iter()) {
          if self.check_pattern_internal(
            elem_pat,
            elem_ty,
            env,
            _ctx,
            require_full_struct,
          ) == PatternRefutability::Refutable
          {
            refutable = PatternRefutability::Refutable;
          }
        }

        if is_struct { refutable } else { PatternRefutability::Refutable }
      }

      PatKind::Path { def_id } => {
        let path_ty = self.check_path(*def_id, env, None);
        let result = self.unify(&path_ty, expected);
        self.handle_unify_result(result, pat.span);

        if self.def_kind_is_struct(*def_id) {
          PatternRefutability::Irrefutable
        } else {
          PatternRefutability::Refutable
        }
      }

      PatKind::Or(pats) => {
        let mut any_irrefutable = false;
        if let Some((first, rest)) = pats.split_first() {
          if self.check_pattern_internal(first, expected, env, _ctx, require_full_struct)
            == PatternRefutability::Irrefutable
          {
            any_irrefutable = true;
          }

          for alt in rest {
            let mut alt_env = env.clone();
            if self.check_pattern_internal(
              alt,
              expected,
              &mut alt_env,
              _ctx,
              require_full_struct,
            ) == PatternRefutability::Irrefutable
            {
              any_irrefutable = true;
            }
          }
        }

        if any_irrefutable {
          PatternRefutability::Irrefutable
        } else {
          PatternRefutability::Refutable
        }
      }

      PatKind::Slice { prefix, middle, suffix } => {
        let elem_ty = self.slice_element_type(expected, pat.span);
        let mut refutable = if let InferTy::Array { len, .. } =
          self.infcx.resolve(expected)
          && matches!(len, ArrayLength::Len(_))
          && self.slice_pattern_fits_array(
            prefix.len(),
            middle.is_some(),
            suffix.len(),
            len,
          ) {
          PatternRefutability::Irrefutable
        } else {
          PatternRefutability::Refutable
        };

        for elem_pat in prefix {
          if self.check_pattern_internal(
            elem_pat,
            &elem_ty,
            env,
            _ctx,
            require_full_struct,
          ) == PatternRefutability::Refutable
          {
            refutable = PatternRefutability::Refutable;
          }
        }

        if let Some(middle) = middle {
          if self.check_pattern_internal(middle, &elem_ty, env, _ctx, require_full_struct)
            == PatternRefutability::Refutable
          {
            refutable = PatternRefutability::Refutable;
          }
        }

        for elem_pat in suffix {
          if self.check_pattern_internal(
            elem_pat,
            &elem_ty,
            env,
            _ctx,
            require_full_struct,
          ) == PatternRefutability::Refutable
          {
            refutable = PatternRefutability::Refutable;
          }
        }

        refutable
      }

      PatKind::Range { start, end, .. } => {
        if let Some(start) = start {
          self.check_expr(start, env, &Expectation::has_type(expected.clone()));
        }
        if let Some(end) = end {
          self.check_expr(end, env, &Expectation::has_type(expected.clone()));
        }

        PatternRefutability::Refutable
      }
    }
  }

  fn tuple_expected_types(
    &self,
    len: usize,
    expected: &InferTy,
    span: Span,
  ) -> Vec<InferTy> {
    let resolved = self.infcx.resolve(expected);
    match resolved {
      InferTy::Tuple(tys, _) if tys.len() == len => tys,
      _ => {
        let fresh: Vec<InferTy> = (0..len).map(|_| self.infcx.fresh(span)).collect();
        let expected_tuple = InferTy::Tuple(fresh.clone(), span);
        let result = self.unify(expected, &expected_tuple);
        self.handle_unify_result(result, span);
        fresh
      }
    }
  }

  fn slice_element_type(&self, expected: &InferTy, span: Span) -> InferTy {
    match self.infcx.resolve(expected) {
      InferTy::Array { ty, .. } | InferTy::Slice(ty, _) => *ty,
      _ => {
        let elem_ty = self.infcx.fresh(span);
        let slice_ty = InferTy::Slice(Box::new(elem_ty.clone()), span);
        let result = self.unify(expected, &slice_ty);
        self.handle_unify_result(result, span);
        elem_ty
      }
    }
  }

  fn slice_pattern_fits_array(
    &self,
    prefix_len: usize,
    has_middle: bool,
    suffix_len: usize,
    len: ArrayLength,
  ) -> bool {
    let expected_len = len.len();
    if has_middle {
      prefix_len + suffix_len <= expected_len
    } else {
      prefix_len + suffix_len == expected_len
    }
  }

  fn def_kind_is_struct(&self, def_id: DefId) -> bool {
    self
      .resolver
      .get_definition(def_id)
      .map(|def| def.kind == DefKind::Struct)
      .unwrap_or(false)
  }

  fn ensure_struct_type(&self, def_id: DefId, expected: &InferTy, span: Span) {
    if let Some(scheme) = self.table.def_type(def_id) {
      let (struct_ty, _subst) = self.instantiate(&scheme);
      let result = self.unify(expected, &struct_ty);
      self.handle_unify_result(result, span);
    }
  }

  fn struct_subst_from_expected(
    &self,
    def_id: DefId,
    expected: &InferTy,
    span: Span,
  ) -> SubstitutionMap {
    let Some(scheme) = self.table.def_type(def_id) else {
      return SubstitutionMap::new();
    };

    if let InferTy::Adt { def_id: expected_id, args, .. } = self.infcx.resolve(expected)
      && expected_id == def_id
      && args.len() == scheme.vars.len()
    {
      let mut subst = SubstitutionMap::new();
      for (var, arg) in scheme.vars.iter().zip(args.iter()) {
        subst.add(*var, arg.clone());
      }
      return subst;
    }

    let (struct_ty, subst) = self.instantiate(&scheme);
    let result = self.unify(expected, &struct_ty);
    self.handle_unify_result(result, span);
    subst
  }
}
