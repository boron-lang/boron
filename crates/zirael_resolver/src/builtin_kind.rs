#[derive(Debug, Clone)]
pub enum BuiltInKind {
  SizeOf,
  AlignOf,
  TypeOf,
  CompileError,
  Unreachable,
}

impl BuiltInKind {
  pub fn try_constructing(name: &str) -> Option<BuiltInKind> {
    match name {
      "sizeOf" => Some(BuiltInKind::SizeOf),
      "alignOf" => Some(BuiltInKind::AlignOf),
      "typeOf" => Some(BuiltInKind::TypeOf),
      "compileError" => Some(BuiltInKind::CompileError),
      "unreachable" => Some(BuiltInKind::Unreachable),

      _ => None,
    }
  }
}
