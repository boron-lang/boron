use std::num::ParseIntError;
use zirael_diagnostic_macro::Diagnostic;
use zirael_diagnostics::DiagnosticCtx;
use zirael_parser::IntBase;

pub fn construct_i128(
  dcx: &DiagnosticCtx,
  base: IntBase,
  value: String,
) -> i128 {
  let radix = base.radix();

  match i128::from_str_radix(&value, radix) {
    Ok(value) => value,
    Err(error) => {
      dcx.emit(IntLitFailedToConstruct { error });

      0
    }
  }
}

#[derive(Diagnostic)]
#[error("failed to construct an int literal: {error}")]
#[code(COMPTIME_FAILED_TO_CONSTRUCT_INT_LIT)]
pub struct IntLitFailedToConstruct {
  error: ParseIntError,
}
