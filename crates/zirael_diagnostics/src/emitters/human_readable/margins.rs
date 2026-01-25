use crate::emitters::human_readable::label::{LabelInfo, LineLabel};
use crate::emitters::human_readable::{CROSS_GAPS, HumanReadableEmitter};
use crate::fmt::Fmt;
use std::io;
use std::io::Write;
use std::ops::Range;
use zirael_source::prelude::SourceFile;
use zirael_source::span::Span;

impl<'a> HumanReadableEmitter {
  pub fn write_margin(
    &self,
    w: &mut dyn Write,
    idx: usize,
    is_line: bool,
    is_ellipsis: bool,
    draw_labels: bool,
    report_row: Option<(usize, bool)>,
    line_labels: &[LineLabel<'a>],
    margin_label: &Option<LineLabel<'a>>,
    line_no_width: usize,
    multi_labels_with_message: &[&LabelInfo<'a>],
    src: &SourceFile,
  ) -> io::Result<()> {
    self.write_line_number(w, idx, is_line, is_ellipsis, line_no_width)?;

    if draw_labels {
      self.write_multi_line_margins(
        w,
        idx,
        is_line,
        is_ellipsis,
        report_row,
        line_labels,
        margin_label,
        multi_labels_with_message,
        src,
      )?;
    }

    Ok(())
  }

  fn write_line_number(
    &self,
    w: &mut dyn Write,
    idx: usize,
    is_line: bool,
    is_ellipsis: bool,
    line_no_width: usize,
  ) -> io::Result<()> {
    let draw = &self.characters;

    let line_no_margin = if is_line && !is_ellipsis {
      let line_no = format!("{}", idx + 1);
      let padding = " ".repeat(line_no_width - line_no.chars().count());
      format!("{}{} {}", padding, line_no, draw.vbar).fg(self.margin_color())
    } else {
      let padding = " ".repeat(line_no_width + 1);
      let vbar = if is_ellipsis { draw.vbar_gap } else { draw.vbar };
      format!("{}{}", padding, vbar).fg(self.skipped_margin_color())
    };

    write!(w, " {line_no_margin} ")
  }

  fn write_multi_line_margins(
    &self,
    w: &mut dyn Write,
    idx: usize,
    is_line: bool,
    is_ellipsis: bool,
    report_row: Option<(usize, bool)>,
    line_labels: &[LineLabel<'a>],
    margin_label: &Option<LineLabel<'a>>,
    multi_labels_with_message: &[&LabelInfo<'a>],
    src: &SourceFile,
  ) -> io::Result<()> {
    let col_count =
      multi_labels_with_message.len() + (!multi_labels_with_message.is_empty()) as usize;

    for col in 0..col_count {
      let chars = self.get_margin_chars(
        col,
        idx,
        is_line,
        is_ellipsis,
        report_row,
        line_labels,
        margin_label,
        multi_labels_with_message,
        src,
      );
      write!(w, "{}{}", chars.0, chars.1)?;
    }

    Ok(())
  }

  fn get_margin_chars(
    &self,
    col: usize,
    idx: usize,
    is_line: bool,
    is_ellipsis: bool,
    report_row: Option<(usize, bool)>,
    line_labels: &[LineLabel<'a>],
    margin_label: &Option<LineLabel<'a>>,
    multi_labels_with_message: &[&LabelInfo<'a>],
    src: &SourceFile,
  ) -> (String, String) {
    let draw = &self.characters;
    let multi_label = multi_labels_with_message.get(col);
    let line_span = src.line(idx).unwrap().span();

    let mut state = MarginState::default();

    // Collect information about what to draw
    for (i, label) in multi_labels_with_message
      [0..(col + 1).min(multi_labels_with_message.len())]
      .iter()
      .enumerate()
    {
      self.update_margin_state(
        &mut state,
        label,
        i,
        col,
        &line_span,
        is_line,
        report_row,
        line_labels,
        margin_label,
      );
    }

    // Apply margin pointer logic
    if let (Some((margin, _)), true) = (state.margin_ptr, is_line) {
      let is_col = multi_label.is_some_and(|ml| std::ptr::eq(*ml, margin.label));
      let is_limit = col + 1 == multi_labels_with_message.len();
      if !is_col && !is_limit {
        state.hbar = state.hbar.or(Some(margin.label));
      }
    }

    // Filter hbar for margin labels
    state.hbar = state.hbar.filter(|l| {
      margin_label.as_ref().is_none_or(|margin| !std::ptr::eq(margin.label, *l))
        || !is_line
    });

    self.render_margin_chars(
      state,
      multi_label,
      col,
      is_line,
      is_ellipsis,
      multi_labels_with_message,
    )
  }

  fn update_margin_state(
    &self,
    state: &mut MarginState<'a>,
    label: &'a LabelInfo<'a>,
    i: usize,
    col: usize,
    line_span: &Span,
    is_line: bool,
    report_row: Option<(usize, bool)>,
    line_labels: &[LineLabel<'a>],
    margin_label: &'a Option<LineLabel<'a>>,
  ) {
    let is_parent = i != col;
    let is_start = line_span.contains(label.char_span.start);
    let is_end = line_span.contains(label.last_offset());
    let overlaps =
      label.char_span.start <= line_span.end && label.char_span.end > line_span.start;

    if !overlaps {
      return;
    }

    let margin = margin_label.as_ref().filter(|m| std::ptr::eq(label, m.label));

    if let Some(margin) = margin.filter(|_| is_line) {
      state.margin_ptr = Some((margin, is_start));
    } else if !is_start && (!is_end || is_line) {
      state.vbar = state.vbar.or(Some(label).filter(|_| !is_parent));
    } else if let Some((report_row, is_arrow)) = report_row {
      self.handle_report_row(
        state,
        label,
        margin,
        report_row,
        is_arrow,
        is_start,
        is_parent,
        line_labels,
        col,
        i,
      );
    }
  }

  fn handle_report_row(
    &self,
    state: &mut MarginState<'a>,
    label: &'a LabelInfo<'a>,
    margin: Option<&LineLabel<'a>>,
    report_row: usize,
    is_arrow: bool,
    is_start: bool,
    is_parent: bool,
    line_labels: &[LineLabel<'a>],
    col: usize,
    i: usize,
  ) {
    let label_row = line_labels
      .iter()
      .enumerate()
      .find(|(_, l)| std::ptr::eq(label, l.label))
      .map_or(0, |(r, _)| r);

    if report_row == label_row {
      if let Some(margin) = margin {
        state.vbar = Some(margin.label).filter(|_| col == i);
        if is_start {
          return;
        }
      }

      if is_arrow {
        state.hbar = Some(label);
        if !is_parent {
          state.corner = Some((label, is_start));
        }
      } else if !is_start {
        state.vbar = state.vbar.or(Some(label).filter(|_| !is_parent));
      }
    } else {
      state.vbar = state
        .vbar
        .or(Some(label).filter(|_| !is_parent && (is_start ^ (report_row < label_row))));
    }
  }

  fn render_margin_chars(
    &self,
    state: MarginState<'a>,
    multi_label: Option<&&LabelInfo<'a>>,
    col: usize,
    is_line: bool,
    is_ellipsis: bool,
    multi_labels_with_message: &[&LabelInfo<'a>],
  ) -> (String, String) {
    let draw = &self.characters;

    // Handle corner case
    if let Some((label, is_start)) = state.corner {
      let char_a = if is_start { draw.ltop } else { draw.lbot };
      return (
        char_a.fg(label.info.color()).to_string(),
        draw.hbar.fg(label.info.color()).to_string(),
      );
    }

    // Handle hbar with vbar
    if let Some(label) = state.hbar.filter(|_| state.vbar.is_some() && !CROSS_GAPS) {
      return (
        draw.xbar.fg(label.info.color()).to_string(),
        draw.hbar.fg(label.info.color()).to_string(),
      );
    }

    // Handle hbar alone
    if let Some(label) = state.hbar {
      return (
        draw.hbar.fg(label.info.color()).to_string(),
        draw.hbar.fg(label.info.color()).to_string(),
      );
    }

    // Handle vbar
    if let Some(label) = state.vbar {
      let vbar_char = if is_ellipsis { draw.vbar_gap } else { draw.vbar };
      return (vbar_char.fg(label.info.color()).to_string(), ' '.fg(None).to_string());
    }

    // Handle margin pointer
    if let (Some((margin, is_start)), true) = (state.margin_ptr, is_line) {
      let is_col = multi_label.is_some_and(|ml| std::ptr::eq(*ml, margin.label));
      let is_limit = col == multi_labels_with_message.len();

      let char_a = if is_limit {
        draw.rarrow
      } else if is_col {
        if is_start { draw.ltop } else { draw.lcross }
      } else {
        draw.hbar
      };

      let char_b = if !is_limit { draw.hbar } else { ' ' };

      return (
        char_a.fg(margin.label.info.color()).to_string(),
        char_b.fg(margin.label.info.color()).to_string(),
      );
    }

    // Default case
    (' '.fg(None).to_string(), ' '.fg(None).to_string())
  }
}

#[derive(Default)]
struct MarginState<'a> {
  corner: Option<(&'a LabelInfo<'a>, bool)>,
  hbar: Option<&'a LabelInfo<'a>>,
  vbar: Option<&'a LabelInfo<'a>>,
  margin_ptr: Option<(&'a LineLabel<'a>, bool)>,
}
