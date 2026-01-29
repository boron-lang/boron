use crate::Label;
use crate::emitters::human_readable::CONTEXT_LINES;
use std::ops::Range;

pub enum LabelKind {
  Inline,
  Multiline,
}

pub struct LabelInfo<'a> {
  pub kind: LabelKind,
  pub char_span: Range<usize>,
  pub start_line: usize,
  pub end_line: usize,
  pub info: &'a Label,
}

impl LabelInfo<'_> {
  pub fn last_offset(&self) -> usize {
    self.char_span.end.saturating_sub(1).max(self.char_span.start)
  }

  pub fn display_range(&self) -> Range<usize> {
    self.start_line.saturating_sub(CONTEXT_LINES)..self.end_line + CONTEXT_LINES + 1
  }
}

pub struct LineLabel<'a> {
  pub col: usize,
  pub label: &'a LabelInfo<'a>,
  pub multi: bool,
  pub draw_msg: bool,
}
