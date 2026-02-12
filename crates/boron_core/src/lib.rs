mod check;
mod unit;

pub mod vars {
  pub const FILE_EXTENSION: &str = "bo";
}

pub mod prelude {
  pub use crate::{check::*, unit::*, vars::*};
  pub use boron_diagnostics::prelude::*;
  pub use boron_parser::*;
  pub use boron_session::{prelude::*, *};
}
