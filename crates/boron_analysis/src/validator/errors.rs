use boron_diagnostic_macro::Diagnostic;
use boron_utils::prelude::Span;

#[derive(Diagnostic)]
#[error("compile time functions don't take generic parameters")]
#[code(COMPTIME_NO_GENERICS)]
#[help("for working with types use type parameters")]
pub struct ComptimeNoGenerics {
  #[error("found here")]
  pub span: Span,
}
