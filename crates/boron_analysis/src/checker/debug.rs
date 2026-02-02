use crate::checker::TyChecker;

impl TyChecker<'_> {
  pub fn debug_print_resolved_types(&self) {
    println!("\n=== Substitution Map ===");
    for entry in &self.infcx.substitution {
      let var = entry.key();
      let ty = entry.value();
      println!("  {:?} -> {:?}", var, self.format_type(ty));
    }

    println!("\n=== Type Variable Kinds ===");
    for entry in &self.infcx.var_kinds {
      let var = entry.key();
      let kind = entry.value();
      println!("  {var:?} => {kind:?}");
    }

    println!("\n=== Resolved Node Types ===");
    for entry in &self.table.node_types {
      let hir_id = entry.key();
      let ty = entry.value();
      let resolved = self.infcx.resolve(ty);
      println!("  {:?} => {:?}", hir_id, self.format_type(&resolved));
    }
    println!("\n=== Type Schemes ===");
    for entry in &self.table.def_types {
      let def_id = entry.key();
      let scheme = entry.value();
      println!("  {def_id:?} => {scheme:?}");
    }
    println!();

    println!("\n=== Monomorphizations ===");
    for mono in &self.table.monomorphizations {
      let def_id = mono.key();
      let scheme = mono.value();
      println!("  {def_id:?} => {scheme:?}");
    }
    println!();
  }
}
