mod chars;
mod label;
mod margins;
mod source_groups;

use crate::Diag;
use crate::emitters::Emitter;
use crate::emitters::human_readable::chars::{Characters, ascii};
use crate::emitters::human_readable::label::{LabelInfo, LabelKind, LineLabel};
use crate::emitters::human_readable::source_groups::SourceGroup;
use crate::fmt::Fmt;
use crate::show::Show;
use std::io;
use std::io::Write;
use std::sync::Arc;
use unicode_width::UnicodeWidthChar;
use yansi::Color;
use zirael_source::prelude::{SourceFile, Sources, Span};

pub const CONTEXT_LINES: usize = 0;
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

  fn advice_color(&self) -> Option<Color> {
    Some(Color::Fixed(147)).filter(|_| self.color)
  }
  fn margin_color(&self) -> Option<Color> {
    Some(Color::Fixed(246)).filter(|_| self.color)
  }
  fn skipped_margin_color(&self) -> Option<Color> {
    Some(Color::Fixed(240)).filter(|_| self.color)
  }
  fn unimportant_color(&self) -> Option<Color> {
    Some(Color::Fixed(249)).filter(|_| self.color)
  }
  fn note_color(&self) -> Option<Color> {
    Some(Color::Fixed(115)).filter(|_| self.color)
  }
  fn filter_color(&self, color: Option<Color>) -> Option<Color> {
    color.filter(|_| self.color)
  }

  fn char_width(&self, c: char, col: usize) -> (char, usize) {
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
  fn emit_diagnostic(&self, diag: &Diag, w: &mut dyn Write) -> anyhow::Result<()> {
    let draw = self.characters.clone();

    // --- Header ---
    let code = diag.code.as_ref().map(|c| format!("[{c}] "));
    let id = format!("{}{}:", Show(code), diag.level.name());
    let kind_color = diag.level.color();
    writeln!(w, "{} {}", id.fg(kind_color), diag.message)?;

    let groups = self.get_source_groups(diag);

    // Line number maximum width
    let line_no_width = groups
      .iter()
      .filter_map(|SourceGroup { char_span, src_id, .. }| {
        let src_name = self
          .sources
          .display(*src_id)
          .map(|d| d.to_string())
          .unwrap_or_else(|| "<unknown>".to_owned());

        let src = if let Some(src) = self.sources.get(*src_id) {
          src
        } else {
          eprintln!("Unable to fetch source {src_name}");
          return None;
        };

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
      .unwrap_or(0);

    // --- Source sections ---
    let groups_len = groups.len();
    for (group_idx, SourceGroup { src_id, char_span, labels, .. }) in
      groups.into_iter().enumerate()
    {
      let src_name = self
        .sources
        .display(src_id)
        .map(|d| d.to_string())
        .unwrap_or_else(|| "<unknown>".to_owned());

      let src = if let Some(src) = self.sources.get(src_id) {
        src
      } else {
        eprintln!("Unable to fetch source {src_name}");
        continue;
      };

      let line_range = src.get_line_range(char_span);

      // File name & reference
      let location = labels[0].char_span.start;
      let line_and_col = src.get_byte_line(location).map(|(line_obj, idx, col)| {
        let line_text = src.get_line_text(line_obj).unwrap();

        let col_chars =
          line_text.char_indices().take_while(|(byte_idx, _)| *byte_idx < col).count();
        (line_obj, idx, col_chars)
      });

      let (line_no, col_no) = line_and_col
        .map(|(_, idx, col)| {
          (format!("{}", idx + 1 + src.display_line_offset()), format!("{}", col + 1))
        })
        .unwrap_or_else(|| ('?'.to_string(), '?'.to_string()));
      let line_ref = format!("{src_name}:{line_no}:{col_no}");

      writeln!(
        w,
        "{}{}{}{} {} {}",
        Show((' ', line_no_width + 2)),
        if group_idx == 0 { draw.ltop } else { draw.lcross }.fg(self.margin_color()),
        draw.hbar.fg(self.margin_color()),
        draw.lbox.fg(self.margin_color()),
        line_ref,
        draw.rbox.fg(self.margin_color()),
      )?;

      writeln!(
        w,
        "{}{}",
        Show((' ', line_no_width + 2)),
        draw.vbar.fg(self.margin_color())
      )?;

      // Generate a list of multi-line labels
      let mut multi_labels = Vec::new();
      let mut multi_labels_with_message = Vec::new();
      for label_info in &labels {
        if matches!(label_info.kind, LabelKind::Multiline) {
          multi_labels.push(label_info);
          if label_info.info.message.is_some() {
            multi_labels_with_message.push(label_info);
          }
        }
      }

      // Sort multiline labels by length
      multi_labels.sort_by_key(|m| {
        -(Span::no_file(m.char_span.start, m.char_span.end).len() as isize)
      });
      multi_labels_with_message.sort_by_key(|m| {
        -(Span::no_file(m.char_span.start, m.char_span.end).len() as isize)
      });

      let mut is_ellipsis = false;
      for idx in line_range {
        let line = if let Some(line) = src.line(idx) {
          line
        } else {
          continue;
        };

        let margin_label = multi_labels_with_message
          .iter()
          .filter_map(|label| {
            let is_start = line.span().contains(label.char_span.start);
            let is_end = line.span().contains(label.last_offset());
            if is_start {
              // TODO: Check to see whether multi is the first on the start line or first on the end line
              Some(LineLabel {
                col: label.char_span.start - line.offset(),
                label,
                multi: true,
                draw_msg: false, // Multi-line spans don;t have their messages drawn at the start
              })
            } else if is_end {
              Some(LineLabel {
                col: label.last_offset() - line.offset(),
                label,
                multi: true,
                draw_msg: true, // Multi-line spans have their messages drawn at the end
              })
            } else {
              None
            }
          })
          .min_by_key(|ll| (ll.col, !ll.label.char_span.start));

        // Generate a list of labels for this line, along with their label columns
        let mut line_labels = multi_labels_with_message
          .iter()
          .filter_map(|label| {
            let is_start = line.span().contains(label.char_span.start);
            let is_end = line.span().contains(label.last_offset());
            if is_start
              && margin_label.as_ref().is_none_or(|m| !std::ptr::eq(*label, m.label))
            {
              // TODO: Check to see whether multi is the first on the start line or first on the end line
              Some(LineLabel {
                col: label.char_span.start - line.offset(),
                label,
                multi: true,
                draw_msg: false, // Multi-line spans don;t have their messages drawn at the start
              })
            } else if is_end {
              Some(LineLabel {
                col: label.last_offset() - line.offset(),
                label,
                multi: true,
                draw_msg: true, // Multi-line spans have their messages drawn at the end
              })
            } else {
              None
            }
          })
          .collect::<Vec<_>>();

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

        // Skip this line if we don't have labels for it
        if line_labels.is_empty() && margin_label.is_none() {
          let within_label = multi_labels
            .iter()
            .any(|label| label.char_span.contains(&line.span().start()));
          if !is_ellipsis && within_label {
            is_ellipsis = true;
          } else {
            if !is_ellipsis {
              self.write_margin(
                w,
                idx,
                false,
                is_ellipsis,
                false,
                None,
                &[],
                &None,
                line_no_width,
                &multi_labels_with_message,
                &src,
              )?;
              writeln!(w)?;
            }
            is_ellipsis = true;
            continue;
          }
        } else {
          is_ellipsis = false;
        }

        // Sort the labels by their columns
        line_labels
          .sort_by_key(|ll| (ll.label.info.order, ll.col, !ll.label.char_span.start));

        // Determine label bounds so we know where to put error messages
        let arrow_end_space = 2;
        let arrow_len = line_labels.iter().fold(0, |l, ll| {
          if ll.multi {
            line.len()
          } else {
            l.max(ll.label.char_span.end.saturating_sub(line.offset()))
          }
        }) + arrow_end_space;

        // Should we draw a vertical bar as part of a label arrow on this line?
        let get_vbar = |col, row| {
          line_labels
            .iter()
            // Only labels with notes get an arrow
            .enumerate()
            .filter(|(_, ll)| {
              ll.label.info.message.is_some()
                && margin_label.as_ref().is_none_or(|m| !std::ptr::eq(ll.label, m.label))
            })
            .find(|(j, ll)| ll.col == col && row <= *j)
            .map(|(_, ll)| ll)
        };

        let get_underline = |col| {
          line_labels
            .iter()
            .filter(|ll| {
              UNDERLINES

                    // Underlines only occur for inline spans (highlighting can occur for all spans)
                    && !ll.multi
                    && ll.label.char_span.contains(&(line.offset() + col))
            })
            // Prioritise displaying smaller spans
            .min_by_key(|ll| {
              (-ll.label.info.priority, ExactSizeIterator::len(&ll.label.char_span))
            })
        };

        // Margin

        self.write_margin(
          w,
          idx,
          true,
          is_ellipsis,
          true,
          None,
          &line_labels,
          &margin_label,
          line_no_width,
          &multi_labels_with_message,
          &src,
        )?;

        // Line
        if !is_ellipsis {
          for (col, c) in src.get_line_text(line).unwrap().trim_end().chars().enumerate()
          {
            let (c, width) = self.char_width(c, col);
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

        // Arrows
        for row in 0..line_labels.len() {
          let line_label = &line_labels[row];
          //No message to draw thus no arrow to draw
          if line_label.label.info.message.is_none() {
            continue;
          }
          // Margin alternate
          self.write_margin(
            w,
            idx,
            false,
            is_ellipsis,
            true,
            Some((row, false)),
            &line_labels,
            &margin_label,
            line_no_width,
            &multi_labels_with_message,
            &src,
          )?;

          // Lines alternate
          let mut chars = src.get_line_text(line).unwrap().trim_end().chars();
          for col in 0..arrow_len {
            let width = chars.next().map_or(1, |c| self.char_width(c, col).1);

            let vbar = get_vbar(col, row);
            let underline = get_underline(col).filter(|_| row == 0);
            let [c, tail] = if let Some(vbar_ll) = vbar {
              let [c, tail] = if underline.is_some() {
                // TODO: Is this good?
                // The `true` is used here because it's temporarily disabling a
                // feature that might be reenabled later.
                #[expect(clippy::overly_complex_bool_expr)]
                if ExactSizeIterator::len(&vbar_ll.label.char_span) <= 1 || true {
                  [draw.underbar, draw.underline]
                } else if line.offset() + col == vbar_ll.label.char_span.start {
                  [draw.ltop, draw.underbar]
                } else if line.offset() + col == vbar_ll.label.last_offset() {
                  [draw.rtop, draw.underbar]
                } else {
                  [draw.underbar, draw.underline]
                }
              } else if vbar_ll.multi && row == 0 && MULTILINE_ARROWS {
                [draw.uarrow, ' ']
              } else {
                [draw.vbar, ' ']
              };
              [c.fg(vbar_ll.label.info.color()), tail.fg(vbar_ll.label.info.color())]
            } else if let Some(underline_ll) = underline {
              [draw.underline.fg(underline_ll.label.info.color()); 2]
            } else {
              [' '.fg(None); 2]
            };

            for i in 0..width {
              write!(w, "{}", if i == 0 { c } else { tail })?;
            }
          }
          writeln!(w)?;

          // Margin
          self.write_margin(
            w,
            idx,
            false,
            is_ellipsis,
            true,
            Some((row, true)),
            &line_labels,
            &margin_label,
            line_no_width,
            &multi_labels_with_message,
            &src,
          )?;
          // Lines
          let mut chars = src.get_line_text(line).unwrap().trim_end().chars();
          for col in 0..arrow_len {
            let width = chars.next().map_or(1, |c| self.char_width(c, col).1);

            let is_hbar = (((col > line_label.col) ^ line_label.multi)
              || (line_label.label.info.message.is_some()
                && line_label.draw_msg
                && col > line_label.col))
              && line_label.label.info.message.is_some();
            let [c, tail] = if col == line_label.col
              && line_label.label.info.message.is_some()
              && margin_label
                .as_ref()
                .is_none_or(|m| !std::ptr::eq(line_label.label, m.label))
            {
              [
                if line_label.multi {
                  if line_label.draw_msg { draw.mbot } else { draw.rbot }
                } else {
                  draw.lbot
                }
                .fg(line_label.label.info.color()),
                draw.hbar.fg(line_label.label.info.color()),
              ]
            } else if let Some(vbar_ll) = get_vbar(col, row).filter(|_| {
              col != line_label.col || line_label.label.info.message.is_some()
            }) {
              if !CROSS_GAPS && is_hbar {
                [
                  draw.xbar.fg(line_label.label.info.color()),
                  ' '.fg(line_label.label.info.color()),
                ]
              } else if is_hbar {
                [draw.hbar.fg(line_label.label.info.color()); 2]
              } else {
                [
                  if vbar_ll.multi && row == 0 { draw.uarrow } else { draw.vbar }
                    .fg(vbar_ll.label.info.color()),
                  ' '.fg(line_label.label.info.color()),
                ]
              }
            } else if is_hbar {
              [draw.hbar.fg(line_label.label.info.color()); 2]
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
            write!(w, " {}", Show(line_label.label.info.message.as_ref()))?;
          }
          writeln!(w)?;
        }
      }

      let is_final_group = group_idx + 1 == groups_len;

      // Help
      if is_final_group {
        for (i, help) in diag.helps.iter().enumerate() {
          self.write_margin(
            w,
            0,
            false,
            false,
            true,
            Some((0, false)),
            &[],
            &None,
            line_no_width,
            &multi_labels_with_message,
            &src,
          )?;
          writeln!(w)?;

          let help_prefix = format!("{} {}", "Help", i + 1);
          let help_prefix_len = if diag.helps.len() > 1 { help_prefix.len() } else { 4 };
          let mut lines = help.lines();
          if let Some(line) = lines.next() {
            self.write_margin(
              w,
              0,
              false,
              false,
              true,
              Some((0, false)),
              &[],
              &None,
              line_no_width,
              &multi_labels_with_message,
              &src,
            )?;
            if diag.helps.len() > 1 {
              writeln!(w, "{}: {}", help_prefix.fg(self.note_color()), line)?;
            } else {
              writeln!(w, "{}: {}", "Help".fg(self.note_color()), line)?;
            }
          }
          for line in lines {
            self.write_margin(
              w,
              0,
              false,
              false,
              true,
              Some((0, false)),
              &[],
              &None,
              line_no_width,
              &multi_labels_with_message,
              &src,
            )?;
            writeln!(w, "{:>pad$}{}", "", line, pad = help_prefix_len + 2)?;
          }
        }
      }

      // Note
      if is_final_group {
        for (i, note) in diag.notes.iter().enumerate() {
          self.write_margin(
            w,
            0,
            false,
            false,
            true,
            Some((0, false)),
            &[],
            &None,
            line_no_width,
            &multi_labels_with_message,
            &src,
          )?;
          writeln!(w)?;

          let note_prefix = format!("{} {}", "Note", i + 1);
          let note_prefix_len = if diag.notes.len() > 1 { note_prefix.len() } else { 4 };
          let mut lines = note.lines();
          if let Some(line) = lines.next() {
            self.write_margin(
              w,
              0,
              false,
              false,
              true,
              Some((0, false)),
              &[],
              &None,
              line_no_width,
              &multi_labels_with_message,
              &src,
            )?;
            if diag.notes.len() > 1 {
              writeln!(w, "{}: {}", note_prefix.fg(self.note_color()), line)?;
            } else {
              writeln!(w, "{}: {}", "Note".fg(self.note_color()), line)?;
            }
          }
          for line in lines {
            self.write_margin(
              w,
              0,
              false,
              false,
              true,
              Some((0, false)),
              &[],
              &None,
              line_no_width,
              &multi_labels_with_message,
              &src,
            )?;
            writeln!(w, "{:>pad$}{}", "", line, pad = note_prefix_len + 2)?;
          }
        }
      }

      if is_final_group {
        let final_margin =
          format!("{}{}", Show((draw.hbar, line_no_width + 2)), draw.rbot);
        writeln!(w, "{}", final_margin.fg(self.margin_color()))?;
      } else {
        writeln!(
          w,
          "{}{}",
          Show((' ', line_no_width + 2)),
          draw.vbar.fg(self.margin_color())
        )?;
      }
    }
    Ok(())
  }

  fn sources(&self) -> &Arc<Sources> {
    &self.sources
  }
}
