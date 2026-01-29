mod check;
mod unit;

pub mod vars {
  pub const FILE_EXTENSION: &str = "zr";
}

pub mod prelude {
  pub use crate::{check::*, unit::*, vars::*};
  pub use boron_diagnostics::prelude::*;
  pub use boron_parser::*;
  pub use boron_source::prelude::*;
  pub use boron_utils::context::*;
  pub use boron_utils::{prelude::*, *};
}
