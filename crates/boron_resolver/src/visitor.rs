use crate::builtin_kind::BuiltInKind;
use crate::def::{DefKind, Definition};
use crate::errors::{
  DuplicateDefinition, InvalidPathRoot, PrivateItem, UndefinedName, UndefinedNameInModule,
};
use crate::module_resolver::ModuleResolver;
use crate::resolver::Resolver;
use crate::scope::ScopeKind;
use crate::symbol::{Symbol, SymbolKind};
use crate::DefId;
use boron_parser::ast::expressions::{Expr, ExprKind};
use boron_parser::ast::items::{
  ConstItem, EnumItem, FunctionItem, Item, ItemKind, ModItem, StructItem, Visibility,
};
use boron_parser::ast::params::Param;
use boron_parser::ast::statements::{Block, Statement};
use boron_parser::ast::types::Type;
use boron_parser::ast::ProgramNode;
use boron_parser::module::Modules;
use boron_parser::{
  ComptimeArg, ElseBranch, GenericParams, IfExpr, NodeId, Path, StructMember,
  VariantField, VariantPayload,
};
use boron_source::prelude::{SourceFileId, Span};
use boron_utils::context::Context;
use boron_utils::prelude::{get_or_intern, Identifier};

#[derive(Clone, Debug, Copy, Hash, PartialEq, Eq)]
pub enum Namespace {
  Value,
  Type,
}

pub struct ResolveVisitor<'a> {
  pub module_resolver: ModuleResolver<'a>,
  pub ctx: &'a Context<'a>,
}

impl<'a> ResolveVisitor<'a> {
  pub fn new(
    resolver: &'a Resolver,
    ctx: &'a Context<'a>,
    current_file: SourceFileId,
  ) -> Self {
    Self { module_resolver: ModuleResolver::new(resolver, current_file), ctx }
  }

  pub fn resolve_modules(
    resolver: &'a Resolver,
    modules: &Modules,
    ctx: &'a Context<'a>,
  ) {
    // collect all top-level definitions
    for module in modules.all() {
      let mut visitor = ResolveVisitor::new(resolver, ctx, module.source_file_id);
      visitor.collect_definitions(&module.node);
    }

    let order = match resolver.import_graph.resolution_order() {
      Ok(order) => order,
      Err(err) => {
        ctx.dcx().emit(err);
        return;
      }
    };

    // resolve all references
    for file_id in &order {
      let Some(module) = modules.get(*file_id) else {
        continue;
      };

      let mut visitor = ResolveVisitor::new(resolver, ctx, *file_id);
      visitor.resolve_module(&module.node);
    }
  }

  pub fn current_file(&self) -> SourceFileId {
    self.module_resolver.current_file
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

  fn collect_item(&mut self, item: &Item) {
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
        self.ctx.dcx().emit(DuplicateDefinition { name, span, previous: def.span });
        return None;
      }
    }

    let def =
      Definition::new(name.clone(), node_id, self.current_file(), kind, span, vis);
    let def_id = self.resolver().add_definition(def);

    match namespace {
      Namespace::Value => self.module_resolver.define_value(name.clone(), def_id),
      Namespace::Type => self.module_resolver.define_type(name.clone(), def_id),
    }

    self.resolver().symbols.record_resolution(node_id, def_id);

    if let Some(scope_id) = self.module_resolver.current_scope() {
      let symbol_kind = match namespace {
        Namespace::Value => SymbolKind::Value,
        Namespace::Type => SymbolKind::Type,
      };
      let symbol = Symbol::new(name.clone(), def_id, symbol_kind, scope_id);
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

    for param in &func.params {
      let (name, id, span) = match param {
        Param::Regular(reg) => (reg.name, reg.id, reg.span),
        Param::Variadic(var) => (var.name, var.id, var.span),
        Param::SelfParam(s) => (get_or_intern("self", Some(s.span)), s.id, s.span),
      };

      let def =
        Definition::new(name.clone(), id, self.current_file(), DefKind::Param, span, vis);
      let def_id = self.resolver().add_definition(def);
      self.module_resolver.define_value(name, def_id);
      self.module_resolver.resolver.symbols.record_resolution(id, def_id);
    }
  }

  fn define_struct(&mut self, s: &StructItem, vis: Visibility) {
    self.define_item(s.name, s.id, DefKind::Struct, *s.name.span(), vis, Namespace::Type);

    for member in &s.members {
      if let StructMember::Item(i) = member {
        self.collect_item(i);
      }
    }
  }

  fn define_enum(&mut self, e: &EnumItem, vis: Visibility) {
    self.define_item(e.name, e.id, DefKind::Enum, *e.name.span(), vis, Namespace::Type);
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
            self.resolver().export_inline_value(module_def_id, name, def_id)
          }
          Namespace::Type => {
            self.resolver().export_inline_type(module_def_id, name, def_id)
          }
        }
      }
    }
  }

  fn resolve_module(&mut self, node: &ProgramNode) {
    if !self.module_resolver.restore_module_rib() {
      self.module_resolver.enter_scope(ScopeKind::Module, None);
      for item in &node.items {
        self.collect_item(item);
      }
    }

    for import in &node.imports {
      self.resolve_import(import);
    }

    for item in &node.items {
      self.resolve_item(item);
    }

    self.module_resolver.leave_scope();
  }

  fn resolve_item(&mut self, item: &Item) {
    match &item.kind {
      ItemKind::Function(func) => self.resolve_function(func, item),
      ItemKind::Struct(s) => self.resolve_struct(s, item),
      ItemKind::Enum(e) => self.resolve_enum(e, item),
      ItemKind::Const(c) => self.resolve_const(c),
      ItemKind::Mod(m) => self.resolve_mod(m),
    }
  }

  fn resolve_function(&mut self, func: &FunctionItem, item: &Item) {
    let func_def = self.module_resolver.lookup_value(&func.name);
    self.module_resolver.enter_scope(ScopeKind::Function, func_def);
    self.resolve_generics(item, &func.generics);

    for param in &func.params {
      self.resolve_param(param);
    }

    self.resolve_type(&func.return_type);

    if let Some(body) = &func.body {
      self.resolve_block(body);
    }

    self.module_resolver.leave_scope();
  }

  fn resolve_param(&mut self, param: &Param) {
    match param {
      Param::SelfParam(_) => {}
      Param::Regular(regular) => {
        self.resolve_type(&regular.ty);
      }
      Param::Variadic(variadic) => {
        self.resolve_type(&variadic.ty);
      }
    }
  }

  fn resolve_generics(&mut self, item: &Item, generics: &Option<GenericParams>) {
    if let Some(generics) = &generics {
      for param in &generics.params {
        let span = *param.name.span();
        let def = Definition::new(
          param.name,
          param.id,
          self.current_file(),
          DefKind::TypeParam,
          span,
          item.visibility,
        );
        let def_id = self.resolver().add_definition(def);
        self.resolver().symbols.record_resolution(param.id, def_id);
        self.module_resolver.define_type(param.name, def_id);
      }
    }
  }

  fn resolve_struct(&mut self, s: &StructItem, item: &Item) {
    let struct_def = self.module_resolver.lookup_type(&s.name);
    self.module_resolver.enter_scope(ScopeKind::Struct, struct_def);
    self.resolve_generics(item, &s.generics);

    for member in &s.members {
      match member {
        StructMember::Field(field) => {
          self.resolve_type(&field.ty);
        }
        StructMember::Item(item) => self.resolve_item(item),
      }
    }

    self.module_resolver.leave_scope();
  }

  fn resolve_enum(&mut self, e: &EnumItem, item: &Item) {
    let enum_def = self.module_resolver.lookup_type(&e.name);
    self.module_resolver.enter_scope(ScopeKind::Enum, enum_def);
    self.resolve_generics(item, &e.generics);

    for variant in &e.variants {
      if let Some(payload) = &variant.payload {
        match payload {
          VariantPayload::Tuple(fields) => {
            for field in fields {
              match field {
                VariantField::Named { ty, .. } | VariantField::Unnamed(ty) => {
                  self.resolve_type(ty);
                }
              }
            }
          }
          VariantPayload::Discriminant(expr) => {
            self.resolve_expr(expr);
          }
        }
      }
    }

    self.module_resolver.leave_scope();
  }

  fn resolve_const(&mut self, c: &ConstItem) {
    self.resolve_type(&c.ty);
    self.resolve_expr(&c.value);
  }

  fn resolve_mod(&mut self, m: &ModItem) {
    let mod_def = self.module_resolver.lookup_value(&m.name);
    self.module_resolver.enter_scope(ScopeKind::Module, mod_def);

    for item in &m.items {
      self.resolve_item(item);
    }

    self.module_resolver.leave_scope();
  }

  fn resolve_block(&mut self, block: &Block) {
    self.module_resolver.enter_scope(ScopeKind::Block, None);

    for stmt in &block.statements {
      self.resolve_statement(stmt);
    }

    self.module_resolver.leave_scope();
  }

  fn resolve_statement(&mut self, stmt: &Statement) {
    match stmt {
      Statement::VarDecl(var) => {
        self.resolve_expr(&var.value);

        if let Some(ty) = &var.ty {
          self.resolve_type(ty);
        }

        let span = *var.name.span();
        let def = Definition::new(
          var.name,
          var.id,
          self.current_file(),
          DefKind::Local,
          span,
          Visibility::Public(Span::dummy()),
        );
        let def_id = self.resolver().add_definition(def);
        self.module_resolver.define_value(var.name, def_id);
      }
      Statement::ConstDecl(c) => {
        self.resolve_expr(&c.value);
        if let Some(ty) = &c.ty {
          self.resolve_type(ty);
        }

        let span = *c.name.span();
        let def = Definition::new(
          c.name,
          c.id,
          self.current_file(),
          DefKind::Const,
          span,
          Visibility::Public(Span::dummy()),
        );
        let def_id = self.resolver().add_definition(def);
        self.module_resolver.define_value(c.name, def_id);
      }
      Statement::Expr(expr_stmt) => {
        self.resolve_expr(&expr_stmt.expr);
      }
      Statement::Block(block) => {
        self.resolve_block(block);
      }
    }
  }

  fn resolve_if_expr(&mut self, if_expr: &IfExpr) {
    self.resolve_expr(&if_expr.condition);
    self.resolve_block(&if_expr.then_block);
    if let Some(else_branch) = &if_expr.else_branch {
      match else_branch {
        ElseBranch::Block(block) => {
          self.resolve_block(block);
        }
        ElseBranch::If(else_if) => {
          self.resolve_if_expr(else_if);
        }
      }
    }
  }

  fn resolve_path(&mut self, path: &Path) {
    let id = path.id;
    let span = path.span;
    if path.segments.is_empty() {
      return;
    }

    for segment in &path.segments {
      for arg in &segment.args {
        self.resolve_type(arg);
      }
    }

    if let Some(root) = &path.root {
      self.ctx.dcx().emit(InvalidPathRoot { root: root.to_string(), span });
      return;
    }

    if path.segments.len() == 1 {
      let name = path.segments[0].identifier;

      if let Some(def_id) = self
        .module_resolver
        .lookup_value(&name)
        .or_else(|| self.module_resolver.lookup_type(&name))
      {
        self.resolver().symbols.record_resolution(id, def_id);
      } else {
        self.ctx.dcx().emit(UndefinedName { name, span });
      }
      return;
    }

    let first = &path.segments[0];
    let first_name = first.identifier;

    let Some(mut current_def) = self
      .module_resolver
      .lookup_value(&first_name)
      .or_else(|| self.module_resolver.lookup_type(&first_name))
    else {
      self
        .ctx
        .dcx()
        .emit(UndefinedName { name: first_name, span: *first.identifier.span() });
      return;
    };

    for segment in &path.segments[1..] {
      let seg_name = segment.identifier;
      let Some(def) = self.resolver().get_definition(current_def) else {
        return;
      };

      let module_name = path
        .segments
        .iter()
        .take_while(|s| !std::ptr::eq(*s, segment))
        .map(|s| s.identifier.text())
        .collect::<Vec<_>>()
        .join("::");

      match def.kind {
        DefKind::Module => {
          if let Some(def_id) = self
            .resolver()
            .lookup_inline_module_value(current_def, &seg_name)
            .or_else(|| self.resolver().lookup_inline_module_type(current_def, &seg_name))
          {
            current_def = def_id;
          } else if let Some(def_id) = self
            .resolver()
            .lookup_module_value(def.source_file, &seg_name)
            .or_else(|| self.resolver().lookup_module_type(def.source_file, &seg_name))
          {
            current_def = def_id;
          } else {
            self.ctx.dcx().emit(UndefinedNameInModule {
              name: seg_name,
              module_name,
              span: *segment.identifier.span(),
            });
            return;
          }
        }
        DefKind::Enum | DefKind::Struct => {
          break;
        }
        _ => {
          break;
        }
      }
    }

    if let Some(def) = self.resolver().get_definition(current_def) {
      if self.current_file() != def.source_file && def.visibility.is_private() {
        self.ctx.dcx().emit(PrivateItem { name: def.name, span: path.span })
      }
    }

    self.resolver().symbols.record_resolution(id, current_def);
  }

  fn resolve_expr(&mut self, expr: &Expr) {
    match &expr.kind {
      ExprKind::Path(path) => self.resolve_path(path),

      ExprKind::Binary { left, right, .. } => {
        self.resolve_expr(left);
        self.resolve_expr(right);
      }

      ExprKind::Assign { target, value, .. } => {
        self.resolve_expr(target);
        self.resolve_expr(value);
      }

      ExprKind::Ternary { condition, then_expr, else_expr } => {
        self.resolve_expr(condition);
        self.resolve_expr(then_expr);
        self.resolve_expr(else_expr);
      }

      ExprKind::Cast { expr, target_type } => {
        self.resolve_expr(expr);
        self.resolve_type(target_type);
      }

      ExprKind::Call { callee, args } => {
        self.resolve_expr(callee);
        for arg in args {
          self.resolve_expr(&arg.value);
        }
      }

      ExprKind::Field { object, .. } => {
        self.resolve_expr(object);
      }

      ExprKind::Index { object, index } => {
        self.resolve_expr(object);
        self.resolve_expr(index);
      }

      ExprKind::AddrOf { operand, .. } | ExprKind::Unary { operand, .. } => {
        self.resolve_expr(operand);
      }

      ExprKind::Struct { path, fields } => {
        self.resolve_path(path);
        for field in fields {
          self.resolve_expr(&field.value);
        }
      }

      ExprKind::If(if_expr) => {
        self.resolve_if_expr(if_expr);
      }

      ExprKind::Match(match_expr) => {
        self.resolve_expr(&match_expr.scrutinee);
        for arm in &match_expr.arms {
          // Pattern binding would go here
          self.resolve_expr(&arm.body);
        }
      }

      ExprKind::Block(block) => {
        self.resolve_block(block);
      }

      ExprKind::Loop(loop_expr) => {
        self.module_resolver.enter_scope(ScopeKind::Loop, None);
        self.resolve_block(&loop_expr.body);
        self.module_resolver.leave_scope();
      }

      ExprKind::While(while_expr) => {
        self.resolve_expr(&while_expr.condition);
        self.module_resolver.enter_scope(ScopeKind::Loop, None);
        self.resolve_block(&while_expr.body);
        self.module_resolver.leave_scope();
      }

      ExprKind::For(for_expr) => {
        self.resolve_expr(&for_expr.iterator);
        self.module_resolver.enter_scope(ScopeKind::Loop, None);

        let span = *for_expr.binding.span();
        let def = Definition::new(
          for_expr.binding,
          for_expr.id,
          self.current_file(),
          DefKind::Local,
          span,
          Visibility::Public(Span::dummy()),
        );
        let def_id = self.resolver().add_definition(def);
        self.module_resolver.define_value(for_expr.binding, def_id);

        self.resolve_block(&for_expr.body);
        self.module_resolver.leave_scope();
      }

      ExprKind::Break(break_expr) => {
        if let Some(value) = &break_expr.value {
          self.resolve_expr(value);
        }
      }

      ExprKind::Return(return_expr) => {
        if let Some(value) = &return_expr.value {
          self.resolve_expr(value);
        }
      }

      ExprKind::Range(range_expr) => {
        if let Some(start) = &range_expr.start {
          self.resolve_expr(start);
        }
        if let Some(end) = &range_expr.end {
          self.resolve_expr(end);
        }
      }

      ExprKind::Array { values: exprs, repeat } => {
        for e in exprs {
          self.resolve_expr(e);
        }

        if let Some(repeat) = repeat {
          self.resolve_expr(repeat);
        }
      }

      ExprKind::Tuple(exprs) => {
        for e in exprs {
          self.resolve_expr(e);
        }
      }

      ExprKind::Comptime { callee, args } => {
        if let ExprKind::Path(p) = &callee.kind
          && p.segments.len() == 1
        {
          let name = p.segments[0].identifier.text();

          if let Some(kind) = BuiltInKind::try_constructing(&name) {
            self.resolver().record_comptime_builtin(expr.id, kind);
          } else {
            self.resolve_expr(callee);
          }
        } else {
          self.resolve_expr(callee);
        }

        for arg in args {
          match arg {
            ComptimeArg::Expr(e) => {
              self.resolve_expr(e);
            }
            ComptimeArg::Type(t) => {
              self.resolve_type(t);
            }
          }
        }
      }

      ExprKind::Literal(_) | ExprKind::SelfValue | ExprKind::Continue(_) => {}
    }
  }

  fn resolve_type(&mut self, ty: &Type) {
    match ty {
      Type::Path(type_path) => {
        self.resolve_path(type_path);
      }
      Type::Function(func_ty) => {
        for param in &func_ty.params {
          self.resolve_type(param);
        }
        self.resolve_type(&func_ty.return_type);
      }
      Type::Pointer(ptr) => {
        self.resolve_type(&ptr.inner);
      }
      Type::Optional(opt) => {
        self.resolve_type(&opt.inner);
      }
      Type::Array(arr) => {
        self.resolve_type(&arr.element);
        self.resolve_expr(&arr.size);
      }
      Type::Tuple(tuple) => {
        for elem in &tuple.elements {
          self.resolve_type(elem);
        }
      }
      Type::Primitive(_) | Type::Unit(_) | Type::Never(_) | Type::Invalid => {}
    }
  }
}
