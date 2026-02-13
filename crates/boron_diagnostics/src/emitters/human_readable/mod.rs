mod chars;
mod label;
mod margins;
mod source_groups;

use crate::emitters::fmt::Fmt as _;
use crate::emitters::human_readable::chars::{ascii, Characters};
use crate::emitters::human_readable::label::{LabelInfo, LabelKind, LineLabel};
use crate::emitters::human_readable::margins::{MarginContext, MarginLabelContext};
use crate::emitters::human_readable::source_groups::SourceGroup;
use crate::emitters::show::Show;
use crate::emitters::Emitter;
use crate::{Diag, DiagnosticLevel};
use anyhow::Result;
use boron_source::line::Line;
use boron_source::prelude::{SourceFile, Sources, Span};
use std::io::Write;
use std::ops::Range;
use std::sync::Arc;
use unicode_width::UnicodeWidthChar as _;
use yansi::Color;

pub const CONTEXT_LINES: usize = 3;
pub const TAB_WIDTH: usize = 4;
pub const MULTILINE_ARROWS: bool = true;
pub const CROSS_GAPS: bool = true;
pub const UNDERLINES: bool = true;

#[derive(Debug)]
pub struct HumanReadableEmitter {
  characters: Characters,
  color: bool,
  sources: Arc<Sources>,
}

impl HumanReadableEmitter {
  pub fn new(sources: Arc<Sources>, color: bool) -> Self {
    Self { characters: ascii(), color, sources }
  }

  fn margin_color(&self) -> Color {
    Color::Fixed(44)
  }
  fn line_number_color(&self) -> Color {
    Color::Fixed(246)
  }
  fn note_color(&self) -> Color {
    Color::Fixed(115)
  }
  fn help_color(&self) -> Color {
    Color::Fixed(51)
  }

  fn char_width(c: char, col: usize) -> (char, usize) {
    match c {
      '\t' => {
        // Find the column that the tab should end at
        let tab_end = (col / TAB_WIDTH + 1) * TAB_WIDTH;
        (' ', tab_end - col)
      }
      c if c.is_whitespace() => (' ', 1),
      _ => (c, c.width().unwrap_or(1)),
    }
  }
}

impl Emitter for HumanReadableEmitter {
  fn emit_diagnostic(&self, diag: &Diag, w: &mut dyn Write) -> Result<()> {
    Self::write_header(diag, w)?;
    let groups = self.get_source_groups(diag);
    let line_no_width = self.calculate_line_number_width(&groups);
    self.write_source_sections(diag, &groups, line_no_width, w)?;
    Ok(())
  }

  fn sources(&self) -> &Arc<Sources> {
    &self.sources
  }
}

struct LineRenderContext<'a> {
  idx: usize,
  line: &'a Line,
  line_labels: &'a [LineLabel<'a>],
  margin_label: &'a Option<LineLabel<'a>>,
  is_ellipsis: bool,
  line_no_width: usize,
  multi_labels: &'a [&'a LabelInfo<'a>],
  multi_labels_with_message: &'a [&'a LabelInfo<'a>],
  src: &'a SourceFile,
}

impl<'a> HumanReadableEmitter {
  fn margin_context(
    idx: usize,
    is_line: bool,
    is_ellipsis: bool,
    line_no_width: usize,
    multi_labels_with_message: &'a [&'a LabelInfo<'a>],
    src: &'a SourceFile,
  ) -> MarginContext<'a> {
    MarginContext {
      idx,
      is_line,
      is_ellipsis,
      line_no_width,
      multi_labels_with_message,
      src,
    }
  }

  fn label_context(
    draw_labels: bool,
    report_row: Option<(usize, bool)>,
    line_labels: &'a [LineLabel<'a>],
    margin_label: &'a Option<LineLabel<'a>>,
  ) -> MarginLabelContext<'a> {
    MarginLabelContext { draw_labels, report_row, line_labels, margin_label }
  }

  fn write_section_margin(
    &self,
    multi_labels_with_message: &'a [&'a LabelInfo<'a>],
    src: &'a SourceFile,
    line_no_width: usize,
    w: &mut dyn Write,
  ) -> Result<()> {
    let margin = Self::margin_context(
      0,
      false,
      false,
      line_no_width,
      multi_labels_with_message,
      src,
    );
    let labels = Self::label_context(true, Some((0, false)), &[], &None);
    Ok(self.write_margin(w, &margin, &labels)?)
  }

  fn write_section_margin_with_bar(
    &self,
    bar_char: char,
    bar_color: Color,
    multi_labels_with_message: &'a [&'a LabelInfo<'a>],
    src: &'a SourceFile,
    line_no_width: usize,
    w: &mut dyn Write,
  ) -> Result<()> {
    let margin = Self::margin_context(
      0,
      false,
      false,
      line_no_width,
      multi_labels_with_message,
      src,
    );
    let labels = Self::label_context(true, Some((0, false)), &[], &None);
    Ok(self.write_margin_with_bar(w, &margin, &labels, bar_char, bar_color)?)
  }

  fn write_margin_from_ctx(
    &self,
    ctx: &LineRenderContext<'a>,
    is_line: bool,
    draw_labels: bool,
    report_row: Option<(usize, bool)>,
    line_labels: &'a [LineLabel<'a>],
    margin_label: &'a Option<LineLabel<'a>>,
    w: &mut dyn Write,
  ) -> Result<()> {
    let margin = Self::margin_context(
      ctx.idx,
      is_line,
      ctx.is_ellipsis,
      ctx.line_no_width,
      ctx.multi_labels_with_message,
      ctx.src,
    );
    let labels = Self::label_context(draw_labels, report_row, line_labels, margin_label);
    Ok(self.write_margin(w, &margin, &labels)?)
  }

  fn write_header(diag: &Diag, w: &mut dyn Write) -> Result<()> {
    let code = diag.code.as_ref().map(|c| format!("[{c}] "));
    let id = format!("{}{}:", Show(code), diag.level.name());
    let kind_color = diag.level.color();
    writeln!(w, "{} {}", id.fg(kind_color).bold(), diag.message.clone().bold())?;
    Ok(())
  }

  fn calculate_line_number_width(&self, groups: &[SourceGroup<'a>]) -> usize {
    groups
      .iter()
      .filter_map(|SourceGroup { char_span, src_id, .. }| {
        let src = self.sources.get(*src_id)?;
        let line_range = src.get_line_range(char_span.clone());
        Some(
          (1..)
            .map(|x| 10u32.pow(x))
            .take_while(|x| line_range.end as u32 / x != 0)
            .count()
            + 1,
        )
      })
      .max()
      .unwrap_or(0)
  }

  fn write_source_sections(
    &self,
    diag: &Diag,
    groups: &[SourceGroup<'a>],
    line_no_width: usize,
    w: &mut dyn Write,
  ) -> Result<()> {
    let groups_len = groups.len();
    for (group_idx, group) in groups.iter().enumerate() {
      self.write_source_group(diag, group, group_idx, groups_len, line_no_width, w)?;
    }
    Ok(())
  }

  fn write_source_group(
    &self,
    diag: &Diag,
    group: &'a SourceGroup<'a>,
    group_idx: usize,
    groups_len: usize,
    line_no_width: usize,
    w: &mut dyn Write,
  ) -> Result<()> {
    let SourceGroup { src_id, char_span, labels, .. } = group;
    let src_name = self
      .sources
      .display(*src_id)
      .map(|d| d.to_string())
      .unwrap_or_else(|| "<unknown>".to_owned());

    let Some(src) = self.sources.get(*src_id) else {
      eprintln!("Unable to fetch source {:?}", src_id);
      return Ok(());
    };

    self.write_file_reference(group_idx, labels, &src, &src_name, line_no_width, w)?;

    let (multi_labels, multi_labels_with_message) = Self::collect_multi_labels(labels);
    let line_range = src.get_line_range(char_span.clone());

    self.write_lines(
      &line_range,
      labels,
      &multi_labels,
      &multi_labels_with_message,
      &src,
      line_no_width,
      w,
    )?;

    let is_final_group = group_idx + 1 == groups_len;
    if is_final_group {
      self.write_help_and_notes(
        diag,
        &multi_labels_with_message,
        &src,
        line_no_width,
        w,
      )?;
    }

    self.write_group_footer(is_final_group, line_no_width, w)?;
    Ok(())
  }

  fn write_file_reference(
    &self,
    group_idx: usize,
    labels: &[LabelInfo<'a>],
    src: &SourceFile,
    src_name: &str,
    line_no_width: usize,
    w: &mut dyn Write,
  ) -> Result<()> {
    let draw = &self.characters;
    let location = labels[0].char_span.start;
    let (line_no, col_no) = Self::get_line_col_display(src, location);
    let line_ref = format!("{src_name}:{line_no}:{col_no}");

    writeln!(
      w,
      "{}{}{}{}{} {}",
      Show((' ', line_no_width + 1)),
      draw.hbar.fg(self.margin_color()),
      draw.hbar.fg(self.margin_color()),
      draw.hbar.fg(self.margin_color()),
      ">".fg(self.margin_color()),
      line_ref,
    )?;

    writeln!(
      w,
      "{}{}",
      Show((' ', line_no_width + 2)),
      draw.vbar.fg(self.margin_color())
    )?;
    Ok(())
  }

  fn get_line_col_display(src: &SourceFile, location: usize) -> (String, String) {
    src
      .get_byte_line(location)
      .map(|(line_obj, idx, col)| {
        let line_text = src.get_line_text(line_obj).expect("line text should exist");
        let col_chars =
          line_text.char_indices().take_while(|(byte_idx, _)| *byte_idx < col).count();
        (format!("{}", idx + 1 + src.display_line_offset()), format!("{}", col_chars + 1))
      })
      .unwrap_or_else(|| ('?'.to_string(), '?'.to_string()))
  }

  fn collect_multi_labels(
    labels: &'a [LabelInfo<'a>],
  ) -> (Vec<&'a LabelInfo<'a>>, Vec<&'a LabelInfo<'a>>) {
    let mut multi_labels = Vec::new();
    let mut multi_labels_with_message = Vec::new();

    for label_info in labels {
      if matches!(label_info.kind, LabelKind::Multiline) {
        multi_labels.push(label_info);
        if label_info.info.message.is_some() {
          multi_labels_with_message.push(label_info);
        }
      }
    }

    multi_labels.sort_by_key(|m| {
      -(Span::no_file(m.char_span.start, m.char_span.end).len() as isize)
    });
    multi_labels_with_message.sort_by_key(|m| {
      -(Span::no_file(m.char_span.start, m.char_span.end).len() as isize)
    });

    (multi_labels, multi_labels_with_message)
  }

  fn write_lines(
    &self,
    line_range: &Range<usize>,
    labels: &'a [LabelInfo<'a>],
    multi_labels: &[&LabelInfo<'a>],
    multi_labels_with_message: &[&'a LabelInfo<'a>],
    src: &SourceFile,
    line_no_width: usize,
    w: &mut dyn Write,
  ) -> Result<()> {
    let mut is_ellipsis = false;

    for idx in line_range.clone() {
      let Some(line) = src.line(idx) else { continue };

      let margin_label = Self::find_margin_label(&line, multi_labels_with_message);
      let line_labels = Self::collect_line_labels(
        &line,
        labels,
        multi_labels_with_message,
        &margin_label,
      );

      let mut ctx = LineRenderContext {
        idx,
        line: &line,
        line_labels: line_labels.as_slice(),
        margin_label: &margin_label,
        is_ellipsis,
        line_no_width,
        multi_labels,
        multi_labels_with_message,
        src,
      };

      if line_labels.is_empty() && margin_label.is_none() {
        is_ellipsis = self.handle_empty_line(&ctx, w)?;
        continue;
      }

      is_ellipsis = false;
      ctx.is_ellipsis = false;
      self.write_line_with_labels(&ctx, w)?;
    }

    Ok(())
  }

  fn find_margin_label(
    line: &Line,
    multi_labels_with_message: &[&'a LabelInfo<'a>],
  ) -> Option<LineLabel<'a>> {
    multi_labels_with_message
      .iter()
      .filter_map(|label| {
        let is_start = line.span().contains(label.char_span.start);
        let is_end = line.span().contains(label.last_offset());
        if is_start {
          Some(LineLabel {
            col: label.char_span.start - line.offset(),
            label,
            multi: true,
            draw_msg: false,
          })
        } else if is_end {
          Some(LineLabel {
            col: label.last_offset() - line.offset(),
            label,
            multi: true,
            draw_msg: true,
          })
        } else {
          None
        }
      })
      .min_by_key(|ll| (ll.col, !ll.label.char_span.start))
  }

  fn collect_line_labels(
    line: &Line,
    labels: &'a [LabelInfo<'a>],
    multi_labels_with_message: &[&'a LabelInfo<'a>],
    margin_label: &Option<LineLabel<'a>>,
  ) -> Vec<LineLabel<'a>> {
    let mut line_labels = Vec::new();

    // Collect multiline labels
    for label in multi_labels_with_message {
      let is_start = line.span().contains(label.char_span.start);
      let is_end = line.span().contains(label.last_offset());

      if is_start && margin_label.as_ref().is_none_or(|m| !std::ptr::eq(*label, m.label))
      {
        line_labels.push(LineLabel {
          col: label.char_span.start - line.offset(),
          label,
          multi: true,
          draw_msg: false,
        });
      } else if is_end {
        line_labels.push(LineLabel {
          col: label.last_offset() - line.offset(),
          label,
          multi: true,
          draw_msg: true,
        });
      }
    }

    // Collect inline labels
    for label_info in labels.iter().filter(|l| {
      l.char_span.start >= line.span().start && l.char_span.end <= line.span().end
    }) {
      if matches!(label_info.kind, LabelKind::Inline) {
        line_labels.push(LineLabel {
          col: ((label_info.char_span.start + label_info.char_span.end) / 2)
            .max(label_info.char_span.start)
            - line.offset(),
          label: label_info,
          multi: false,
          draw_msg: true,
        });
      }
    }

    line_labels
      .sort_by_key(|ll| (ll.label.info.order, ll.col, !ll.label.char_span.start));
    line_labels
  }

  fn handle_empty_line(
    &self,
    ctx: &LineRenderContext<'a>,
    w: &mut dyn Write,
  ) -> Result<bool> {
    let within_label = ctx
      .multi_labels
      .iter()
      .any(|label| label.char_span.contains(&ctx.line.span().start()));

    if !ctx.is_ellipsis && !within_label {
      let margin = Self::margin_context(
        ctx.idx,
        false,
        ctx.is_ellipsis,
        ctx.line_no_width,
        ctx.multi_labels_with_message,
        ctx.src,
      );
      let labels = Self::label_context(false, None, &[], &None);
      self.write_margin(w, &margin, &labels)?;
      writeln!(w)?;
    }
    Ok(true)
  }

  fn write_line_with_labels(
    &self,
    ctx: &LineRenderContext<'a>,
    w: &mut dyn Write,
  ) -> Result<()> {
    // Write margin and source line
    self.write_margin_from_ctx(
      ctx,
      true,
      true,
      None,
      ctx.line_labels,
      ctx.margin_label,
      w,
    )?;

    if !ctx.is_ellipsis {
      for (col, c) in ctx
        .src
        .get_line_text(*ctx.line)
        .expect("line text should exist")
        .trim_end()
        .chars()
        .enumerate()
      {
        let (c, width) = Self::char_width(c, col);
        if c.is_whitespace() {
          for _ in 0..width {
            write!(w, "{c}")?;
          }
        } else {
          write!(w, "{c}")?;
        }
      }
    }
    writeln!(w)?;

    self.write_label_arrows(ctx, w)?;

    Ok(())
  }

  fn write_label_arrows(
    &self,
    ctx: &LineRenderContext<'a>,
    w: &mut dyn Write,
  ) -> Result<()> {
    let arrow_end_space = 2;
    let arrow_len = ctx.line_labels.iter().fold(0, |l, ll| {
      if ll.multi {
        ctx.line.len()
      } else {
        l.max(ll.label.char_span.end.saturating_sub(ctx.line.offset()))
      }
    }) + arrow_end_space;

    for row in 0..ctx.line_labels.len() {
      let line_label = &ctx.line_labels[row];
      if line_label.label.info.message.is_none() {
        continue;
      }

      self.write_arrow_underline_row(ctx, row, arrow_len, w)?;
      self.write_arrow_message_row(ctx, line_label, row, arrow_len, w)?;
    }

    Ok(())
  }

  fn write_arrow_underline_row(
    &self,
    ctx: &LineRenderContext<'a>,
    row: usize,
    arrow_len: usize,
    w: &mut dyn Write,
  ) -> Result<()> {
    let draw = &self.characters;

    self.write_margin_from_ctx(
      ctx,
      false,
      true,
      Some((row, false)),
      ctx.line_labels,
      ctx.margin_label,
      w,
    )?;

    let mut chars = ctx
      .src
      .get_line_text(*ctx.line)
      .expect("line text should exist")
      .trim_end()
      .chars();
    for col in 0..arrow_len {
      let width = chars.next().map_or(1, |c| Self::char_width(c, col).1);
      let vbar = Self::get_vbar(col, row, ctx.line_labels, ctx.margin_label);
      let underline =
        Self::get_underline(col, ctx.line, ctx.line_labels).filter(|_| row == 0);

      let [c, tail] = if let Some(vbar_ll) = vbar {
        let draw_underline = if vbar_ll.label.info.level == DiagnosticLevel::Help {
          draw.underline_help
        } else {
          draw.underline
        };
            
        let [c, tail] = if underline.is_some() {
          #[expect(clippy::overly_complex_bool_expr)]
          if ExactSizeIterator::len(&vbar_ll.label.char_span) <= 1 || true {
            [draw.underbar, draw_underline]
          } else if ctx.line.offset() + col == vbar_ll.label.char_span.start {
            [draw.ltop, draw.underbar]
          } else if ctx.line.offset() + col == vbar_ll.label.last_offset() {
            [draw.rtop, draw.underbar]
          } else {
            [draw.underbar, draw_underline]
          }
        } else if vbar_ll.multi && row == 0 && MULTILINE_ARROWS {
          [draw.uarrow, ' ']
        } else {
          [draw.vbar, ' ']
        };
        [
          c.fg(vbar_ll.label.info.color()).bold(),
          tail.fg(vbar_ll.label.info.color()).bold(),
        ]
      } else if let Some(underline_ll) = underline {
        let draw_underline = if underline_ll.label.info.level == DiagnosticLevel::Help {
          draw.underline_help
        } else {
          draw.underline
        };
        [draw_underline.fg(underline_ll.label.info.color()).bold(); 2]
      } else {
        [' '.fg(None); 2]
      };

      for i in 0..width {
        write!(w, "{}", if i == 0 { c } else { tail })?;
      }
    }
    writeln!(w)?;
    Ok(())
  }

  fn write_arrow_message_row(
    &self,
    ctx: &LineRenderContext<'a>,
    line_label: &LineLabel<'a>,
    row: usize,
    arrow_len: usize,
    w: &mut dyn Write,
  ) -> Result<()> {
    let draw = &self.characters;

    self.write_margin_from_ctx(
      ctx,
      false,
      true,
      Some((row, true)),
      ctx.line_labels,
      ctx.margin_label,
      w,
    )?;

    let mut chars = ctx
      .src
      .get_line_text(*ctx.line)
      .expect("line text should exist")
      .trim_end()
      .chars();
    let label_msg = &line_label.label.info.message;
    let label_color = line_label.label.info.color();

    for col in 0..arrow_len {
      let width = chars.next().map_or(1, |c| Self::char_width(c, col).1);

      let is_hbar = (((col > line_label.col) ^ line_label.multi)
        || (label_msg.is_some() && line_label.draw_msg && col > line_label.col))
        && label_msg.is_some();

      let [c, tail] = if col == line_label.col
        && label_msg.is_some()
        && ctx
          .margin_label
          .as_ref()
          .is_none_or(|m| !std::ptr::eq(line_label.label, m.label))
      {
        [
          if line_label.multi {
            if line_label.draw_msg { draw.mbot } else { draw.rbot }
          } else {
            draw.lbot
          }
          .fg(label_color)
          .bold(),
          draw.hbar.fg(label_color).bold(),
        ]
      } else if let Some(vbar_ll) =
        Self::get_vbar(col, row, ctx.line_labels, ctx.margin_label)
          .filter(|_| col != line_label.col || label_msg.is_some())
      {
        if !CROSS_GAPS && is_hbar {
          [draw.xbar.fg(label_color).bold(), ' '.fg(label_color).bold()]
        } else if is_hbar {
          [draw.hbar.fg(label_color); 2]
        } else {
          [
            if vbar_ll.multi && row == 0 { draw.uarrow } else { draw.vbar }
              .fg(vbar_ll.label.info.color())
              .bold(),
            ' '.fg(label_color),
          ]
        }
      } else if is_hbar {
        [draw.hbar.fg(label_color).bold(); 2]
      } else {
        [' '.fg(None); 2]
      };

      if width > 0 {
        write!(w, "{c}")?;
      }
      for _ in 1..width {
        write!(w, "{tail}")?;
      }
    }

    if line_label.draw_msg {
      write!(
        w,
        " {}",
        Show(label_msg.as_ref()).bold().fg(line_label.label.info.color())
      )?;
    }
    writeln!(w)?;
    Ok(())
  }

  fn get_vbar(
    col: usize,
    row: usize,
    line_labels: &'a [LineLabel<'a>],
    margin_label: &Option<LineLabel<'a>>,
  ) -> Option<&'a LineLabel<'a>> {
    line_labels
      .iter()
      .enumerate()
      .filter(|(_, ll)| {
        ll.label.info.message.is_some()
          && margin_label.as_ref().is_none_or(|m| !std::ptr::eq(ll.label, m.label))
      })
      .find(|(j, ll)| ll.col == col && row <= *j)
      .map(|(_, ll)| ll)
  }

  fn get_underline(
    col: usize,
    line: &Line,
    line_labels: &'a [LineLabel<'a>],
  ) -> Option<&'a LineLabel<'a>> {
    line_labels
      .iter()
      .filter(|ll| {
        UNDERLINES && !ll.multi && ll.label.char_span.contains(&(line.offset() + col))
      })
      .min_by_key(|ll| {
        (-ll.label.info.priority, ExactSizeIterator::len(&ll.label.char_span))
      })
  }

  fn write_help_and_notes(
    &self,
    diag: &Diag,
    multi_labels_with_message: &[&LabelInfo<'a>],
    src: &SourceFile,
    line_no_width: usize,
    w: &mut dyn Write,
  ) -> Result<()> {
    self.write_helps(diag, multi_labels_with_message, src, line_no_width, w)?;
    self.write_notes(diag, multi_labels_with_message, src, line_no_width, w)?;
    Ok(())
  }

  fn write_helps( 
    &self,
    diag: &Diag,
    multi_labels_with_message: &[&LabelInfo<'a>],
    src: &SourceFile,
    line_no_width: usize,
    w: &mut dyn Write,
  ) -> Result<()> {
    for (i, help) in diag.helps.iter().enumerate() {
      self.write_section_margin(multi_labels_with_message, src, line_no_width, w)?;
      writeln!(w)?;

      let help_prefix = format!("help {}", i + 1);
      let help_prefix_len = if diag.helps.len() > 1 { help_prefix.len() } else { 4 };

      self.write_multiline_section(
        help,
        &help_prefix,
        help_prefix_len,
        '=',
        self.help_color(),
        multi_labels_with_message,
        src,
        line_no_width,
        w,
      )?;
    }
    Ok(())
  }

  fn write_notes(
    &self,
    diag: &Diag,
    multi_labels_with_message: &[&LabelInfo<'a>],
    src: &SourceFile,
    line_no_width: usize,
    w: &mut dyn Write,
  ) -> Result<()> {
    for (i, note) in diag.notes.iter().enumerate() {
      self.write_section_margin(multi_labels_with_message, src, line_no_width, w)?;
      writeln!(w)?;

      let note_prefix = format!("note {}", i + 1);
      let note_prefix_len = if diag.notes.len() > 1 { note_prefix.len() } else { 4 };

      self.write_multiline_section(
        note,
        &note_prefix,
        note_prefix_len,
        '=',
        self.note_color(),
        multi_labels_with_message,
        src,
        line_no_width,
        w,
      )?;
    }
    Ok(())
  }

  fn write_multiline_section(
    &self,
    text: &str,
    prefix: &str,
    prefix_len: usize,
    bar_char: char,
    bar_color: Color,
    multi_labels_with_message: &[&LabelInfo<'a>],
    src: &SourceFile,
    line_no_width: usize,
    w: &mut dyn Write,
  ) -> Result<()> {
    let mut lines = text.lines();
    if let Some(line) = lines.next() {
      self.write_section_margin_with_bar(
        bar_char,
        bar_color,
        multi_labels_with_message,
        src,
        line_no_width,
        w,
      )?;
      writeln!(w, "{}: {}", prefix.bold(), line)?;
    }

    for line in lines {
      self.write_section_margin(multi_labels_with_message, src, line_no_width, w)?;
      writeln!(w, "{:>pad$}{}", "", line, pad = prefix_len + 2)?;
    }
    Ok(())
  }

  fn write_group_footer(
    &self,
    is_final_group: bool,
    line_no_width: usize,
    w: &mut dyn Write,
  ) -> Result<()> {
    let draw = &self.characters;
    if is_final_group {
      writeln!(w)?;
    } else {
      writeln!(
        w,
        "{}{}",
        Show((' ', line_no_width + 2)),
        draw.vbar.fg(self.margin_color())
      )?;
    }
    Ok(())
  }
}
