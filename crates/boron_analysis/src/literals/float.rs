use boron_diagnostic_macro::Diagnostic;
use boron_diagnostics::DiagnosticCtx;
use boron_source::span::Span;
use rustc_apfloat::ieee::{Double, DoubleS, IeeeFloat};
use std::str::FromStr;

pub fn construct_float(
  dcx: &DiagnosticCtx,
  value: &str,
  span: Span,
) -> IeeeFloat<DoubleS> {
  let result = Double::from_str(value);

  result.unwrap_or_else(|err| {
    dcx.emit(FloatLitFailedToConstruct { span, error: err.0.to_owned() });
    Double::from_str("0.0").expect("impossible")
  })
}

#[derive(Diagnostic)]
#[error("failed to construct a float literal: {error}")]
#[code(COMPTIME_FAILED_TO_CONSTRUCT_FLOAT_LIT)]
pub struct FloatLitFailedToConstruct {
  #[error("in this float literal")]
  pub span: Span,
  error: String,
}
