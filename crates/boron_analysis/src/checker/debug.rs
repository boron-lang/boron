use crate::checker::TyChecker;

impl TyChecker<'_> {
  pub fn debug_print_resolved_types(&self) {
    eprintln!("\n=== Substitution Map ===");
    for entry in self.infcx.substitution.iter() {
      let var = entry.key();
      let ty = entry.value();
      eprintln!("  {:?} -> {:?}", var, self.format_type(ty));
    }

    eprintln!("\n=== Type Variable Kinds ===");
    for entry in self.infcx.var_kinds.iter() {
      let var = entry.key();
      let kind = entry.value();
      eprintln!("  {:?} => {:?}", var, kind);
    }

    eprintln!("\n=== Resolved Node Types ===");
    for entry in self.table.node_types.iter() {
      let hir_id = entry.key();
      let ty = entry.value();
      let resolved = self.infcx.resolve(ty);
      eprintln!("  {:?} => {:?}", hir_id, self.format_type(&resolved));
    }
    eprintln!("\n=== Type Schemes ===");
    for entry in self.table.def_types.iter() {
      let def_id = entry.key();
      let scheme = entry.value();
      eprintln!("  {:?} => {:?}", def_id, scheme);
    }
    eprintln!();

    eprintln!("\n=== Monomorphizations ===");
    for mono in self.table.monomorphizations.iter() {
      let def_id = mono.key();
      let scheme = mono.value();
      eprintln!("  {:?} => {:?}", def_id, scheme);
    }
    eprintln!();
  }
}
