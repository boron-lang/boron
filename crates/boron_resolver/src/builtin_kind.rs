#[derive(Debug, Clone, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub enum BuiltInKind {
  SizeOf,
  AlignOf,
  Os,
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
      "os" => Some(Self::Os),
      _ => None,
    }
  }

  pub fn name(self) -> &'static str {
    match self {
      Self::SizeOf => "sizeOf",
      Self::AlignOf => "alignOf",
      Self::TypeOf => "typeOf",
      Self::CompileError => "compileError",
      Self::Unreachable => "unreachable",
      Self::Os => "os"
    }
  }
}
