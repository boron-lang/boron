use crate::emitters::human_readable::HumanReadableEmitter;
use crate::emitters::human_readable::label::{LabelInfo, LabelKind};
use crate::{Diag, Label};
use boron_source::line::Line;
use boron_source::prelude::{SourceFile, SourceFileId};
use std::ops::Range;

pub struct SourceGroup<'a> {
  pub src_id: SourceFileId,
  pub char_span: Range<usize>,
  pub display_range: Range<usize>,
  pub labels: Vec<LabelInfo<'a>>,
}

impl HumanReadableEmitter {
  pub fn get_source_groups<'a>(&self, diag: &'a Diag) -> Vec<SourceGroup<'a>> {
    let labeled_infos = self.collect_label_infos(diag);
    self.group_labels_by_source(labeled_infos)
  }

  fn collect_label_infos<'a>(
    &self,
    diag: &'a Diag,
  ) -> Vec<(LabelInfo<'a>, SourceFileId)> {
    let mut labeled_infos = Vec::new();

    for label in &diag.labels {
      if let Some(label_info) = self.create_label_info(label) {
        labeled_infos.push((label_info, label.span.file_id));
      }
    }

    labeled_infos.sort_by_key(|(info, _)| {
      (info.info.level, info.info.order, info.end_line, info.start_line)
    });
    labeled_infos
  }

  fn create_label_info<'a>(&self, label: &'a Label) -> Option<LabelInfo<'a>> {
    let source_file = self.sources.get(label.span.file_id);

    if source_file.is_none() {
      eprintln!("Unable to fetch source {:?}", label.span.file_id);
      return None;
    }

    let byte_span = label.span.start()..label.span.end();
    let char_span_and_lines =
      self.compute_char_span_and_lines(&*source_file?, &byte_span)?;

    let (char_span, start_line, end_line) = char_span_and_lines;
    let kind =
      if start_line == end_line { LabelKind::Inline } else { LabelKind::Multiline };

    Some(LabelInfo { kind, char_span, start_line, end_line, info: label })
  }

  fn compute_char_span_and_lines(
    &self,
    source: &SourceFile,
    byte_span: &Range<usize>,
  ) -> Option<(Range<usize>, usize, usize)> {
    if byte_span.start >= byte_span.end {
      let (line_obj, line_num, byte_col) = source.get_byte_line(byte_span.start)?;
      let line_text = source.get_line_text(line_obj).expect("line text should exist");
      let char_offset = self.calculate_char_offset(&line_obj, line_text, byte_col);
      return Some((char_offset..char_offset, line_num, line_num));
    }

    let (start_line_obj, start_line_num, start_byte_col) =
      source.get_byte_line(byte_span.start)?;
    let start_line_text =
      source.get_line_text(start_line_obj).expect("line text should exist");
    let start_char_offset =
      self.calculate_char_offset(&start_line_obj, start_line_text, start_byte_col);

    let end_byte_pos = byte_span.end - 1;
    let (end_line_obj, end_line_num, end_byte_col) =
      source.get_byte_line(end_byte_pos)?;
    let end_line_text =
      source.get_line_text(end_line_obj).expect("line text should exist");

    let end_char_offset =
      self.calculate_char_offset(&end_line_obj, end_line_text, end_byte_col + 1);

    Some((start_char_offset..end_char_offset, start_line_num, end_line_num))
  }

  fn calculate_char_offset(
    &self,
    line: &Line,
    line_text: &str,
    byte_col: usize,
  ) -> usize {
    let safe_byte_col = byte_col.min(line_text.len());
    let chars_before_col = line_text[..safe_byte_col].chars().count();
    line.offset() + chars_before_col
  }

  fn group_labels_by_source<'a>(
    &self,
    labeled_infos: Vec<(LabelInfo<'a>, SourceFileId)>,
  ) -> Vec<SourceGroup<'a>> {
    let mut groups: Vec<SourceGroup<'a>> = Vec::new();

    for (label, src_id) in labeled_infos {
      if self.can_merge_with_last_group(&groups, src_id, &label) {
        self.merge_label_into_last_group(&mut groups, label);
      } else {
        groups.push(self.create_new_group(src_id, label));
      }
    }

    groups
  }

  fn can_merge_with_last_group(
    &self,
    groups: &[SourceGroup<'_>],
    src_id: SourceFileId,
    label: &LabelInfo<'_>,
  ) -> bool {
    groups.last().is_some_and(|group| {
      group.src_id == src_id
        && group.labels.last().is_none_or(|last| last.end_line <= label.end_line)
    })
  }

  fn merge_label_into_last_group<'a>(
    &self,
    groups: &mut [SourceGroup<'a>],
    label: LabelInfo<'a>,
  ) {
    let group = groups.last_mut().expect("checked in can_merge_with_last_group");

    group.char_span.start = group.char_span.start.min(label.char_span.start);
    group.char_span.end = group.char_span.end.max(label.char_span.end);

    let label_display_range = label.display_range();
    group.display_range.start = group.display_range.start.min(label_display_range.start);
    group.display_range.end = group.display_range.end.max(label_display_range.end);

    group.labels.push(label);
  }

  fn create_new_group<'a>(
    &self,
    src_id: SourceFileId,
    label: LabelInfo<'a>,
  ) -> SourceGroup<'a> {
    SourceGroup {
      src_id,
      char_span: label.char_span.clone(),
      display_range: label.display_range(),
      labels: vec![label],
    }
  }
}
