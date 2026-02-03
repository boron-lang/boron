use boron_diagnostic_macro::Diagnostic;
use boron_diagnostics::DiagnosticCtx;
use boron_parser::IntBase;
use boron_source::span::Span;
use std::num::ParseIntError;

pub fn construct_i128(
  dcx: &DiagnosticCtx,
  base: IntBase,
  value: &str,
  span: Span,
) -> i128 {
  let radix = base.radix();

  match i128::from_str_radix(value, radix) {
    Ok(value) => value,
    Err(error) => {
      dcx.emit(IntLitFailedToConstruct { span, error });

      0
    }
  }
}

#[derive(Diagnostic)]
#[error("failed to construct an int literal: {error}")]
#[code(COMPTIME_FAILED_TO_CONSTRUCT_INT_LIT)]
pub struct IntLitFailedToConstruct {
  #[error("in this int literal")]
  pub span: Span,
  error: ParseIntError,
}
