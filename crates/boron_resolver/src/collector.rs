use crate::errors::{DuplicateDefinition, SelfOutsideMethod};
use crate::visitor::Namespace;
use crate::{
    DefId, DefKind, Definition, ResolveVisitor, Resolver, ScopeId, ScopeKind, Symbol,
    SymbolKind,
};
use boron_parser::module::Modules;
use boron_parser::{
    ConstItem, EnumItem, EnumMember, FunctionItem, Item, ItemKind, ModItem, NodeId, Param,
    ProgramNode, StructItem, StructMember, Visibility,
};
use boron_session::prelude::Session;
use boron_source::ident_table::{get_or_intern, Identifier};
use boron_source::prelude::Span;

impl<'a> ResolveVisitor<'a> {
  pub fn resolve_modules(resolver: &'a Resolver, modules: &Modules, sess: &'a Session) {
    // collect all top-level definitions
    for module in modules.all() {
      let mut visitor = ResolveVisitor::new(resolver, sess, module.source_file_id);
      visitor.collect_definitions(&module.node);
    }

    let order = match resolver.import_graph.resolution_order() {
      Ok(order) => order,
      Err(err) => {
        sess.dcx().emit(err);
        return;
      }
    };

    // resolve all references
    for file_id in &order {
      let Some(module) = modules.get(*file_id) else {
        continue;
      };

      let mut visitor = ResolveVisitor::new(resolver, sess, *file_id);
      visitor.resolve_module(&module.node);
    }
  }

  fn collect_definitions(&mut self, node: &ProgramNode) {
    self.module_resolver.enter_scope(ScopeKind::Module, None);

    for import in &node.imports {
      self.collect_import(import);
    }

    for item in &node.items {
      self.collect_item(item);
    }

    self.module_resolver.save_module_rib();
  }

  pub fn collect_item(&mut self, item: &Item) {
    match &item.kind {
      ItemKind::Function(func) => {
        self.define_function(func, item.visibility);
      }
      ItemKind::Struct(s) => {
        self.define_struct(s, item.visibility);
      }
      ItemKind::Enum(e) => {
        self.define_enum(e, item.visibility);
      }
      ItemKind::Const(c) => {
        self.define_const(c, item.visibility);
      }
      ItemKind::Mod(m) => {
        self.define_mod(m, item.visibility);
      }
    }
  }

  pub fn resolver(&self) -> &Resolver {
    self.module_resolver.resolver
  }

  fn define_item(
    &mut self,
    name: Identifier,
    node_id: NodeId,
    kind: DefKind,
    span: Span,
    vis: Visibility,
    namespace: Namespace,
  ) -> Option<DefId> {
    let existing = match namespace {
      Namespace::Value => self.module_resolver.lookup_value(&name),
      Namespace::Type => self.module_resolver.lookup_type(&name),
    };

    if let Some(existing) = existing {
      if let Some(def) = self.resolver().get_definition(existing) {
        self.sess.dcx().emit(DuplicateDefinition { name, span, previous: def.span });
        return None;
      }
    }

    let def = Definition::new(name, node_id, self.current_file(), kind, span, vis);
    let def_id = self.resolver().add_definition(def);

    match namespace {
      Namespace::Value => self.module_resolver.define_value(name, def_id),
      Namespace::Type => self.module_resolver.define_type(name, def_id),
    }

    self.resolver().symbols.record_resolution(node_id, def_id);

    if let Some(scope_id) = self.module_resolver.current_scope() {
      let symbol_kind = match namespace {
        Namespace::Value => SymbolKind::Value,
        Namespace::Type => SymbolKind::Type,
      };
      let symbol = Symbol::new(name, def_id, symbol_kind, scope_id);
      self.resolver().symbols.insert(symbol);
    }

    if self.module_resolver.is_at_module_scope() {
      match namespace {
        Namespace::Value => self.module_resolver.export_value(name, def_id),
        Namespace::Type => self.module_resolver.export_type(name, def_id),
      }
    }

    Some(def_id)
  }
  fn define_function(&mut self, func: &FunctionItem, vis: Visibility) {
    self.define_item(
      func.name,
      func.id,
      DefKind::Function,
      *func.name.span(),
      vis,
      Namespace::Value,
    );
  }

  fn define_struct(&mut self, s: &StructItem, vis: Visibility) {
    let struct_def_id = self.define_item(
      s.name,
      s.id,
      DefKind::Struct,
      *s.name.span(),
      vis,
      Namespace::Type,
    );

    self.module_resolver.enter_scope(ScopeKind::Struct, struct_def_id);

    for member in &s.members {
      if let StructMember::Item(i) = member {
        self.collect_item(i);

        if let Some(struct_def_id) = struct_def_id {
          let (name, node_id) = match &i.kind {
            ItemKind::Function(f) => (f.name, f.id),
            ItemKind::Const(c) => (c.name, c.id),
            _ => continue,
          };
          if let Some(def_id) = self.resolver().symbols.get_resolution(node_id) {
            self.resolver().add_adt_member(struct_def_id, name, def_id);
          }
        }
      }
    }

    self.module_resolver.leave_scope();
  }

  fn define_enum(&mut self, e: &EnumItem, vis: Visibility) {
    let enum_id =
      self.define_item(e.name, e.id, DefKind::Enum, *e.name.span(), vis, Namespace::Type);

    self.module_resolver.enter_scope(ScopeKind::Enum, enum_id);

    for member in &e.members {
      if let Some(enum_def_id) = enum_id {
        if let EnumMember::Item(i) = member {
          self.collect_item(i);

          let (name, node_id) = match &i.kind {
            ItemKind::Function(f) => (f.name, f.id),
            ItemKind::Const(c) => (c.name, c.id),
            _ => continue,
          };
          if let Some(def_id) = self.resolver().symbols.get_resolution(node_id) {
            self.resolver().add_adt_member(enum_def_id, name, def_id);
          }
        } else if let EnumMember::Variant(variant) = member {
          let def = self.define_item(
            variant.name,
            variant.id,
            DefKind::Variant,
            *variant.name.span(),
            Visibility::Public(Span::default()),
            Namespace::Type,
          );

          if let Some(def_id) = def {
            self.resolver().add_adt_member(enum_def_id, variant.name, def_id);
          }
        }
      }
    }

    self.module_resolver.leave_scope();
  }

  fn define_const(&mut self, c: &ConstItem, vis: Visibility) {
    self.define_item(c.name, c.id, DefKind::Const, *c.name.span(), vis, Namespace::Value);
  }

  fn define_mod(&mut self, m: &ModItem, vis: Visibility) {
    let name = m.name;
    let span = *m.name.span();

    let def =
      Definition::new(name, m.id, self.current_file(), DefKind::Module, span, vis);
    let def_id = self.resolver().add_definition(def);

    self.module_resolver.define_value(name, def_id);
    self.module_resolver.define_type(name, def_id);

    self.resolver().symbols.record_resolution(m.id, def_id);

    if let Some(scope_id) = self.module_resolver.current_scope() {
      let symbol = Symbol::new(name, def_id, SymbolKind::Module, scope_id);
      self.resolver().symbols.insert(symbol);
    }

    if matches!(vis, Visibility::Public(_)) && self.module_resolver.is_at_module_scope() {
      self.module_resolver.export_value(name, def_id);
      self.module_resolver.export_type(name, def_id);
    }

    self.module_resolver.enter_scope(ScopeKind::Module, Some(def_id));
    for item in &m.items {
      self.collect_inline_mod_item(item, def_id);
    }
    self.module_resolver.leave_scope();
  }

  fn collect_inline_mod_item(&mut self, item: &Item, module_def_id: DefId) {
    let (name, node_id, def_kind, span, namespace) = match &item.kind {
      ItemKind::Function(f) => {
        (f.name, f.id, DefKind::Function, *f.name.span(), Namespace::Value)
      }
      ItemKind::Struct(s) => {
        (s.name, s.id, DefKind::Struct, *s.name.span(), Namespace::Type)
      }
      ItemKind::Enum(e) => (e.name, e.id, DefKind::Enum, *e.name.span(), Namespace::Type),
      ItemKind::Const(c) => {
        (c.name, c.id, DefKind::Const, *c.name.span(), Namespace::Value)
      }
      ItemKind::Mod(m) => {
        self.define_mod(m, item.visibility);
        if matches!(item.visibility, Visibility::Public(_)) {
          if let Some(nested_def_id) = self.module_resolver.lookup_value(&m.name) {
            self.resolver().export_inline_value(module_def_id, m.name, nested_def_id);
            self.resolver().export_inline_type(module_def_id, m.name, nested_def_id);
          }
        }
        return;
      }
    };

    if let Some(def_id) =
      self.define_item(name, node_id, def_kind, span, item.visibility, namespace)
    {
      if matches!(item.visibility, Visibility::Public(_)) {
        match namespace {
          Namespace::Value => {
            self.resolver().export_inline_value(module_def_id, name, def_id);
          }
          Namespace::Type => {
            self.resolver().export_inline_type(module_def_id, name, def_id);
          }
        }
      }
    }
  }

  pub fn define_param(
    &mut self,
    param: &Param,
    vis: Visibility,
    function_scope: ScopeId,
  ) {
    let (name, id, span) = match param {
      Param::Regular(reg) => (reg.name, reg.id, reg.span),
      Param::Variadic(var) => (var.name, var.id, var.span),
      Param::SelfParam(s) => (get_or_intern("self", Some(s.span)), s.id, s.span),
    };

    let def = Definition::new(name, id, self.current_file(), DefKind::Param, span, vis);
    let def_id = self.resolver().add_definition(def);
    self.module_resolver.define_value(name, def_id);
    self.resolver().symbols.record_resolution(id, def_id);

    if let Param::SelfParam(..) = param {
      if let Some(parent) = self.resolver().parent_scope(function_scope)
        && let Some(scope) = self.resolver().scopes.get(parent)
        && let Some(owner) = scope.owner
      {
        self.resolver().add_self_mapping(def_id, owner);
      } else {
        self.dcx().emit(SelfOutsideMethod { span })
      }
    }
  }
}
