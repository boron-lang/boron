use crate::lower::context::LoweringContext;
use crate::pat::{FieldPat, Pat, PatKind};
use boron_parser::ast::expressions::{FieldPat as AstFieldPat, Pattern, PatternKind};
use boron_utils::prelude::debug;

impl LoweringContext<'_> {
  pub fn lower_ast_pattern(&mut self, pat: &Pattern) -> Pat {
    let (kind, span) = match &pat.kind {
      PatternKind::Wildcard => (PatKind::Wild, pat.span),

      PatternKind::Binding { name, is_mut, subpat } => {
        let def_id = self.get_def_id(pat.id).expect("should have an id");
        let subpat = subpat.as_ref().map(|p| Box::new(self.lower_ast_pattern(p)));

        (PatKind::Binding { def_id, name: *name, is_mut: *is_mut, subpat }, pat.span)
      }

      PatternKind::Literal(lit) => (PatKind::Literal(Self::lower_literal(lit)), pat.span),

      PatternKind::Tuple(patterns) => (
        PatKind::Tuple(patterns.iter().map(|p| self.lower_ast_pattern(p)).collect()),
        pat.span,
      ),

      PatternKind::Struct { path, fields, rest } => {
        let def_id = self.get_def_id(path.id);

        if let Some(def_id) = def_id {
          let fields = fields
            .iter()
            .map(|f: &AstFieldPat| FieldPat {
              hir_id: self.next_hir_id(),
              name: f.name,
              pat: self.lower_ast_pattern(&f.pat),
              span: f.span,
            })
            .collect();

          (PatKind::Struct { def_id, fields, rest: *rest }, pat.span)
        } else {
          debug!("failed to lower struct pattern {pat:#?}");
          (PatKind::Err, pat.span)
        }
      }

      PatternKind::TupleStruct { path, patterns, .. } => {
        let def_id = self.get_def_id(path.id);

        if let Some(def_id) = def_id {
          let pats = patterns.iter().map(|p| self.lower_ast_pattern(p)).collect();
          (PatKind::TupleStruct { def_id, pats }, pat.span)
        } else {
          debug!("failed to lower tuple struct pattern {pat:#?}");
          (PatKind::Err, pat.span)
        }
      }

      PatternKind::Path(path) => {
        let def_id = self.get_def_id(path.id);

        if let Some(def_id) = def_id {
          (PatKind::Path { def_id }, pat.span)
        } else {
          debug!("failed to lower path pattern {pat:#?}");
          (PatKind::Err, pat.span)
        }
      }

      PatternKind::Or(patterns) => (
        PatKind::Or(patterns.iter().map(|p| self.lower_ast_pattern(p)).collect()),
        pat.span,
      ),

      PatternKind::Slice { prefix, middle, suffix } => {
        let prefix = prefix.iter().map(|p| self.lower_ast_pattern(p)).collect();
        let middle = middle.as_ref().map(|p| Box::new(self.lower_ast_pattern(p)));
        let suffix = suffix.iter().map(|p| self.lower_ast_pattern(p)).collect();

        (PatKind::Slice { prefix, middle, suffix }, pat.span)
      }

      PatternKind::Range(range) => {
        let start = range.start.as_ref().map(|e| Box::new(self.lower_expr(e)));
        let end = range.end.as_ref().map(|e| Box::new(self.lower_expr(e)));

        (PatKind::Range { start, end, inclusive: range.inclusive }, pat.span)
      }

      PatternKind::Err => (PatKind::Err, pat.span),
    };

    Pat { hir_id: self.next_hir_id(), kind, span }
  }
}
