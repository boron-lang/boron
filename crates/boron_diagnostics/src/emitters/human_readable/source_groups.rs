use crate::Diag;
use crate::emitters::human_readable::HumanReadableEmitter;
use crate::emitters::human_readable::label::{LabelInfo, LabelKind};
use crate::emitters::show::Show;
use boron_source::prelude::SourceFileId;
use std::ops::Range;

pub struct SourceGroup<'a> {
  pub src_id: SourceFileId,
  pub char_span: Range<usize>,
  pub display_range: Range<usize>,
  pub labels: Vec<LabelInfo<'a>>,
}

impl HumanReadableEmitter {
  pub fn get_source_groups<'a>(&self, diag: &'a Diag) -> Vec<SourceGroup<'a>> {
    let mut labels = Vec::new();
    for label in &diag.labels {
      let label_source = label.span.file_id;

      let src_display = self.sources.display(label_source);
      let Some(src) = self.sources.get(label_source) else {
        eprintln!("Unable to fetch source '{}'", Show(src_display));
        continue;
      };

      let given_label_span = label.span.start()..label.span.end();

      let (label_char_span, start_line, end_line) = {
        let Some((start_line_obj, start_line, start_byte_col)) =
          src.get_byte_line(given_label_span.start)
        else {
          continue;
        };
        let line_text =
          src.get_line_text(start_line_obj).expect("line text should exist");

        let num_chars_before_start =
          line_text[..start_byte_col.min(line_text.len())].chars().count();
        let start_char_offset = start_line_obj.offset() + num_chars_before_start;

        if given_label_span.start >= given_label_span.end {
          (start_char_offset..start_char_offset, start_line, start_line)
        } else {
          // We can subtract 1 from end, because get_byte_line doesn't actually index into the text.
          let end_pos = given_label_span.end - 1;
          let Some((end_line_obj, end_line, end_byte_col)) = src.get_byte_line(end_pos)
          else {
            continue;
          };
          let end_line_text =
            src.get_line_text(end_line_obj).expect("line text should exist");
          // Have to add 1 back now, so we don't cut a char in two.
          let num_chars_before_end = end_line_text[..end_byte_col + 1].chars().count();
          let end_char_offset = end_line_obj.offset() + num_chars_before_end;

          (start_char_offset..end_char_offset, start_line, end_line)
        }
      };

      let label_info = LabelInfo {
        kind: if start_line == end_line {
          LabelKind::Inline
        } else {
          LabelKind::Multiline
        },
        char_span: label_char_span,
        start_line,
        end_line,
        info: label,
      };

      labels.push((label_info, label_source));
    }
    labels.sort_by_key(|(l, _)| (l.info.order, l.end_line, l.start_line));
    let mut groups: Vec<SourceGroup<'a>> = Vec::new();
    for (label, src_id) in labels {
      match groups.last_mut() {
        Some(group)
          if group.src_id == src_id
            && group.labels.last().is_none_or(|last| last.end_line <= label.end_line) =>
        {
          group.char_span.start = group.char_span.start.min(label.char_span.start);
          group.char_span.end = group.char_span.end.max(label.char_span.end);
          let display_range = label.display_range();
          group.display_range.start = group.display_range.start.min(display_range.start);
          group.display_range.end = group.display_range.end.max(display_range.end);
          group.labels.push(label);
        }
        _ => {
          groups.push(SourceGroup {
            src_id,
            char_span: label.char_span.clone(),
            display_range: label.display_range(),
            labels: vec![label],
          });
        }
      }
    }
    groups
  }
}
