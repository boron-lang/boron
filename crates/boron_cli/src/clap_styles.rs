use boron_session::term_style;
use clap::builder::Styles;

pub const CLAP_STYLING: Styles = Styles::styled()
  .header(term_style::HEADER)
  .usage(term_style::USAGE)
  .literal(term_style::LITERAL)
  .placeholder(term_style::PLACEHOLDER)
  .error(term_style::ERROR)
  .valid(term_style::VALID)
  .invalid(term_style::INVALID);
