use log::{Level, LevelFilter};
use std::io::Write;
use yansi::Paint;

pub fn setup_logger(verbose: bool, no_color: bool) {
  if no_color {
    yansi::disable();
  }

  let level = if verbose { LevelFilter::Debug } else { LevelFilter::Info };

  env_logger::Builder::from_default_env()
    .filter_level(level)
    .format(move |buf, record| {
      let level_str = match record.level() {
        Level::Error => "error".bright_red().bold(),
        Level::Warn => "warn ".bright_yellow().bold(),
        Level::Info => "info ".bright_green().bold(),
        Level::Debug => "debug".bright_blue().bold(),
        Level::Trace => "trace".bright_white().bold(),
      };

      let module = if verbose {
        format!(
          " {} ",
          format!("[{}]", record.module_path().unwrap_or("unknown")).black().dim()
        )
      } else {
        String::new()
      };

      writeln!(buf, "{} {}{}", level_str, module, record.args())
    })
    .init();
}
