use std::fmt::{Display, Formatter};
use zirael_parser::ast::NodeId;
use zirael_source::new_id;
use zirael_source::prelude::SourceFileId;
use zirael_utils::prelude::{Identifier, Span};

new_id!(DefId);

/// A definition in the program is the "thing" that a name refers to.
#[derive(Debug, Clone)]
pub struct Definition {
  pub name: String,
  pub id: DefId,
  /// original ast node
  pub node_id: NodeId,
  pub source_file: SourceFileId,
  pub kind: DefKind,
  pub span: Span,
}

impl Definition {
  pub fn new(
    // TODO: this probably should be an identifier
    name: String,
    node_id: NodeId,
    source_file: SourceFileId,
    kind: DefKind,
    span: Span,
  ) -> Self {
    Self { name, id: DefId::new(), node_id, source_file, kind, span }
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DefKind {
  /// A module (file or inline `mod`).
  Module,
  Function,
  Struct,
  Enum,
  /// An enum variant.
  Variant,
  Const,
  /// A local variable binding.
  Local,
  /// A function parameter.
  Param,
  Field,
  Method,
  /// A type parameter (generic).
  TypeParam,
}

impl Display for DefKind {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(
      f,
      "{}",
      match self {
        DefKind::Module => "mod",
        DefKind::Function => "function",
        DefKind::Struct => "struct",
        DefKind::Enum => "enum",
        DefKind::Variant => "variant",
        DefKind::Const => "const",
        DefKind::Local => "local",
        DefKind::Param => "param",
        DefKind::Field => "field",
        DefKind::Method => "method",
        DefKind::TypeParam => "type parameter",
      }
    )
  }
}
