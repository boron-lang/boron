use boron_parser::Visibility;
use boron_parser::ast::NodeId;
use boron_source::new_id;
use boron_source::prelude::SourceFileId;
use boron_utils::prelude::{Identifier, Span};
use std::fmt::{Display, Formatter};

new_id!(DefId);

/// A definition in the program is the "thing" that a name refers to.
#[derive(Debug, Clone)]
pub struct Definition {
  pub name: Identifier,
  pub id: DefId,
  /// original ast node
  pub node_id: NodeId,
  pub source_file: SourceFileId,
  pub kind: DefKind,
  pub span: Span,
  pub visibility: Visibility,
}

impl Definition {
  pub fn new(
    name: Identifier,
    node_id: NodeId,
    source_file: SourceFileId,
    kind: DefKind,
    span: Span,
    visibility: Visibility,
  ) -> Self {
    Self { name, id: DefId::new(), node_id, source_file, kind, span, visibility }
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
        Self::Module => "mod",
        Self::Function => "function",
        Self::Struct => "struct",
        Self::Enum => "enum",
        Self::Variant => "variant",
        Self::Const => "const",
        Self::Local => "local",
        Self::Param => "param",
        Self::Field => "field",
        Self::Method => "method",
        Self::TypeParam => "type parameter",
      }
    )
  }
}
