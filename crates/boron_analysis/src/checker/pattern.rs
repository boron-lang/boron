use crate::checker::TyChecker;
use crate::table::TypeEnv;
use crate::ty::InferTy;
use boron_hir::{Pat, PatKind};

impl TyChecker<'_> {
  pub(crate) fn check_pattern(&mut self, pat: &Pat, expected: &InferTy, env: &mut TypeEnv) {
    match &pat.kind {
      PatKind::Binding { def_id, .. } => {
        env.bind(*def_id, expected.clone());
        self.table.record_node_type(pat.hir_id, expected.clone());
      }
      PatKind::Wild => {
        self.table.record_node_type(pat.hir_id, expected.clone());
      }
      PatKind::Tuple(pats) => {
        if let InferTy::Tuple(tys, _) = expected {
          for (p, ty) in pats.iter().zip(tys.iter()) {
            self.check_pattern(p, ty, env);
          }
        } else {
          // TODO: Report pattern mismatch
        }
      }
      PatKind::Literal(_) => {
        // TODO: Check literal pattern
        self.table.record_node_type(pat.hir_id, expected.clone());
      }
      _ => {
        // TODO: Handle other patterns
      }
    }
  }
}
