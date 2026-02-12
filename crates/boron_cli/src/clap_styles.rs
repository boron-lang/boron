use clap::builder::styling::{AnsiColor, Effects};
use clap::builder::Styles;

pub const CLAP_STYLING: Styles = Styles::styled()
  .header(AnsiColor::Green.on_default().effects(Effects::BOLD))
  .usage(AnsiColor::Green.on_default().effects(Effects::BOLD))
  .literal(AnsiColor::Cyan.on_default().effects(Effects::BOLD))
  .placeholder(AnsiColor::Cyan.on_default())
  .error(AnsiColor::Red.on_default().effects(Effects::BOLD))
  .valid(AnsiColor::Cyan.on_default().effects(Effects::BOLD))
  .invalid(AnsiColor::Yellow.on_default().effects(Effects::BOLD));
