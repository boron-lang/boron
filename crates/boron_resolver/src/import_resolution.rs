use crate::errors::{PrivateItem, UndefinedModule, UndefinedNameInModule};
use crate::{
  DefId, DefKind, Definition, ModuleResolver, ResolveVisitor, Symbol, SymbolKind,
};
use boron_parser::{ImportDecl, ImportKind, ImportSpec, NodeId, Visibility};
use boron_source::ident_table::Identifier;
use boron_source::prelude::{SourceFileId, Span};
use boron_utils::prelude::debug;
use dashmap::DashMap;

impl<'a> ResolveVisitor<'a> {
  pub fn collect_import(&self, import: &ImportDecl) {
    let current = self.sess.dcx().sources().get_unchecked(self.current_file());
    let path = import.path.construct_file(self.sess.root(), current.path().clone());

    let Some(path) = path else { todo!("add proper diagnostic") };

    let importing_from = self.sess.dcx().sources().get_by_path(&path);

    if let Some(source_id) = importing_from {
      let src_id = *source_id.value();
      self.resolver().add_path_mapping(&import.path, src_id);
      self.resolver().add_import_edge(src_id, self.current_file());
    } else {
      self
        .sess
        .dcx()
        .emit(UndefinedModule { name: import.path.to_string(), span: import.path.span });
    }
  }

  pub fn resolve_import(&mut self, import: &ImportDecl) {
    let target_file = self.resolver().lookup_file_for_path(&import.path);

    let Some(target_file) = target_file else {
      debug!("skipping {import:?} because path couldn't be resolved");
      return;
    };

    match &import.kind {
      ImportKind::Wildcard => self.resolve_wildcard_import(target_file, import),
      ImportKind::Items(items) => self.resolve_items_import(target_file, items, import),
      ImportKind::Binding(name) => {
        self.resolve_binding_import(target_file, *name, import);
      }
    }
  }

  fn resolve_binding_import(
    &mut self,
    target_file: SourceFileId,
    binding: Identifier,
    import: &ImportDecl,
  ) {
    let def = Definition::new(
      binding,
      import.id,
      target_file,
      DefKind::Module,
      *binding.span(),
      Visibility::Public(Span::dummy()),
    );
    let def_id = self.resolver().add_definition(def);

    self.module_resolver.define_value(binding, def_id);
    self.module_resolver.define_type(binding, def_id);

    self.resolver().symbols.record_resolution(import.id, def_id);

    if let Some(scope_id) = self.module_resolver.current_scope() {
      let symbol = Symbol::new(binding, def_id, SymbolKind::Module, scope_id);
      self.resolver().symbols.insert(symbol);
    }
  }

  fn resolve_items_import(
    &mut self,
    target_file: SourceFileId,
    specs: &Vec<ImportSpec>,
    import: &ImportDecl,
  ) {
    for spec in specs {
      let (orig_name, local_name) = (spec.name, spec.alias.unwrap_or(spec.name));

      let mut last_def = None;
      let mut recorded = false;

      if let Some(def_id) = self.resolver().lookup_module_value(target_file, &orig_name) {
        self.handle_import_resolution(
          &local_name,
          spec.id,
          def_id,
          &mut recorded,
          ModuleResolver::define_value,
        );
        last_def = Some(def_id);
      }

      if let Some(def_id) = self.resolver().lookup_module_type(target_file, &orig_name) {
        self.handle_import_resolution(
          &local_name,
          spec.id,
          def_id,
          &mut recorded,
          ModuleResolver::define_type,
        );
        last_def = Some(def_id);
      }

      if let Some(def_id) = last_def {
        let def = self.resolver().get_definition(def_id).expect("should exist");

        if def.visibility.is_private() {
          self.sess.dcx().emit(PrivateItem { name: def.name, span: spec.span });
        }
      } else {
        self.sess.dcx().emit(UndefinedNameInModule {
          name: orig_name,
          span: spec.span,
          module_name: import.path.to_string(),
        });
      }
    }
  }

  fn resolve_wildcard_import(&mut self, target_file: SourceFileId, _import: &ImportDecl) {
    if let Some(exports) =
      self.resolver().module_exports_values.get(&target_file).map(|v| v.clone())
    {
      self.define_public_exports(&exports, ModuleResolver::define_value);
    }

    if let Some(exports) =
      self.resolver().module_exports_types.get(&target_file).map(|v| v.clone())
    {
      self.define_public_exports(&exports, ModuleResolver::define_type);
    }
  }

  fn define_public_exports<F>(
    &mut self,
    exports: &DashMap<Identifier, DefId>,
    mut define: F,
  ) where
    F: FnMut(&mut ModuleResolver<'a>, Identifier, DefId),
  {
    for entry in exports {
      let name = *entry.key();
      let def_id = *entry.value();
      let def = self.resolver().get_definition(def_id).expect("should be known");

      if !def.visibility.is_private() {
        define(&mut self.module_resolver, name, def_id);
      }
    }
  }

  fn handle_import_resolution(
    &mut self,
    local_name: &Identifier,
    spec_id: NodeId,
    def_id: DefId,
    recorded: &mut bool,
    define: fn(&mut ModuleResolver<'a>, Identifier, DefId),
  ) {
    define(&mut self.module_resolver, local_name.to_owned(), def_id);
    if !*recorded {
      self.resolver().symbols.record_resolution(spec_id, def_id);
      *recorded = true;
    }
  }
}
