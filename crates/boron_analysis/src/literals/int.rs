use boron_diagnostic_macro::Diagnostic;
use boron_diagnostics::DiagnosticCtx;
use boron_parser::IntBase;
use std::num::ParseIntError;

pub fn construct_i128(dcx: &DiagnosticCtx, base: IntBase, value: &str) -> i128 {
  let radix = base.radix();

  match i128::from_str_radix(value, radix) {
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
