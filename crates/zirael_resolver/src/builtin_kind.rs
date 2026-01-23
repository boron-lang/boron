#[derive(Debug, Clone)]
pub enum BuiltInKind {
  SizeOf,
  AlignOf,
  TypeOf,
  CompileError,
  Unreachable,
}

impl BuiltInKind {
  pub fn try_constructing(name: &str) -> Option<Self> {
    match name {
      "sizeOf" => Some(Self::SizeOf),
      "alignOf" => Some(Self::AlignOf),
      "typeOf" => Some(Self::TypeOf),
      "compileError" => Some(Self::CompileError),
      "unreachable" => Some(Self::Unreachable),

      _ => None,
    }
  }
}
