pub mod dependency;
pub mod enums;
mod errors;
mod module_graph;
mod path;
pub mod project_config;
mod session;

pub mod prelude {
  pub use crate::{path::*, project_config::*, session::*};
  pub use anyhow::{Result, anyhow, bail};
  pub use fs_err as fs;
  pub use log::{debug, error, info, warn};
  pub use parking_lot::*;
  pub use rayon::prelude::*;

  pub use crate::enums::lib_type::*;
  pub use crate::enums::mode::*;
  pub use crate::enums::project_type::*;
  pub use crate::errors::*;
  pub use boron_diagnostics::prelude::*;
  pub use boron_source::ident_table::*;
  pub use boron_source::prelude::*;
  pub use std::{
    collections::{HashMap, HashSet},
    path::PathBuf,
  };
}
