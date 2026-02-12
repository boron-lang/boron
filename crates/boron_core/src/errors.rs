use boron_diagnostic_macro::Diagnostic;
use boron_source::prelude::Span;

#[derive(Diagnostic)]
#[error("couldn't find main function for binary entrypoint in package `{pkg}`")]
#[help("if you intended to compile a library use `--type library`")]
#[code(NO_MAIN_FUNCTION)]
pub struct NoMainFunction {
  pub pkg: String,
}

#[derive(Diagnostic)]
#[error("main function can't contain any generic parameters")]
#[code(MAIN_NO_GENERICS)]
pub struct MainNoGenerics {
  #[error("remove these generic parameters")]
  pub span: Span,
}

#[derive(Diagnostic)]
#[error("main function can't contain any parameters")]
#[code(MAIN_NO_PARAMS)]
pub struct MainNoParams {
  #[error("remove these parameters")]
  pub span: Span,
}
