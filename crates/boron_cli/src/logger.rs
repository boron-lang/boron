use chrono::Local;
use std::fmt;
use tracing::field::Field;
use tracing::span::Record;
use tracing::{Event, Level};
use tracing_subscriber::registry::LookupSpan;
use tracing_subscriber::util::SubscriberInitExt as _;
use tracing_subscriber::{
  EnvFilter,
  fmt::{self as tracing_fmt, FormatEvent, FormatFields, format::Writer},
  layer::SubscriberExt as _,
};
use yansi::Paint as _;

use tracing_subscriber::field::{RecordFields, Visit};
use tracing_subscriber::fmt::FormattedFields;

pub struct CustomFields;

struct FieldVisitor<'a> {
  writer: Writer<'a>,
  is_first: bool,
  result: fmt::Result,
}

impl Visit for FieldVisitor<'_> {
  fn record_str(&mut self, field: &Field, value: &str) {
    self.write_field(field.name(), value);
  }

  fn record_debug(&mut self, field: &Field, value: &dyn fmt::Debug) {
    self.write_field(field.name(), &format!("{value:?}"));
  }
}

impl FieldVisitor<'_> {
  fn write_field(&mut self, key: &str, value: &str) {
    if self.result.is_err() {
      return;
    }
    let sep = if self.is_first { "" } else { " " };
    self.is_first = false;

    if key == "message" {
      self.result = write!(self.writer, "{value}");
    } else if !key.starts_with("log.") {
      self.result =
        write!(self.writer, "{}{}={}", sep, key.dim(), value.white().underline());
    }
  }
}

impl<'writer> FormatFields<'writer> for CustomFields {
  fn format_fields<R: RecordFields>(
    &self,
    writer: Writer<'writer>,
    fields: R,
  ) -> fmt::Result {
    let mut visitor = FieldVisitor { writer, is_first: true, result: Ok(()) };
    fields.record(&mut visitor);
    visitor.result
  }

  fn add_fields(
    &self,
    current: &'writer mut FormattedFields<Self>,
    fields: &Record<'_>,
  ) -> fmt::Result {
    let is_first = current.fields.is_empty();
    let writer = current.as_writer();
    let mut visitor = FieldVisitor { writer, is_first, result: Ok(()) };
    fields.record(&mut visitor);
    visitor.result
  }
}
pub fn setup_logger(verbose: bool, no_color: bool) {
  if no_color {
    yansi::disable();
  }

  let level = if verbose { "debug" } else { "info" };
  let filter = EnvFilter::new(format!("{level},serenity=warn"));

  let fmt_layer = tracing_fmt::layer()
    .fmt_fields(CustomFields)
    .event_format(CustomFormatter { verbose });

  tracing_subscriber::registry().with(filter).with(fmt_layer).init();
}

struct CustomFormatter {
  verbose: bool,
}

impl<S, N> FormatEvent<S, N> for CustomFormatter
where
  S: tracing::Subscriber + for<'a> LookupSpan<'a>,
  N: for<'a> FormatFields<'a> + 'static,
{
  fn format_event(
    &self,
    ctx: &tracing_fmt::FmtContext<'_, S, N>,
    mut writer: Writer<'_>,
    event: &Event<'_>,
  ) -> fmt::Result {
    let meta = event.metadata();

    let ts = Local::now().format("%H:%M:%S");

    let level_str = match *meta.level() {
      Level::ERROR => "error".bright_red().bold(),
      Level::WARN => "warn ".bright_yellow().bold(),
      Level::INFO => "info ".bright_green().bold(),
      Level::DEBUG => "debug".bright_blue().bold(),
      Level::TRACE => "trace".bright_white().bold(),
    };

    write!(writer, "{} {} ", ts.dim(), level_str)?;

    if self.verbose {
      write!(writer, "{} ", format!("[{}]", meta.target()).black().dim())?;
    }

    ctx.format_fields(writer.by_ref(), event)?;
    writeln!(writer)
  }
}
