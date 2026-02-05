use crate::directives::LineDirection;
use boron_core::prelude::{Diagnostic, Sources};

pub(crate) fn matches_directive(
  diagnostic: &Diagnostic,
  directive_line: usize,
  direction: &LineDirection,
  pattern: &str,
  sources: &Sources,
) -> bool {
  let Some(diag_line) = get_diagnostic_line(diagnostic, sources) else {
    return false;
  };

  let line_matches = match direction {
    LineDirection::Up => diag_line <= directive_line,
    LineDirection::Down => diag_line >= directive_line,
  };

  if !line_matches {
    return false;
  }

  matches_pattern(&diagnostic.diag.message, pattern)
}

fn get_diagnostic_line(diagnostic: &Diagnostic, sources: &Sources) -> Option<usize> {
  diagnostic.diag.labels.iter().find_map(|label| {
    let file_id = label.file();
    let src = sources.get(file_id)?;
    src.get_byte_line(label.span.start).map(|(_, line_idx, _)| line_idx)
  })
}

fn matches_pattern(message: &str, pattern: &str) -> bool {
  let parts: Vec<&str> = pattern.split("{}").collect();

  if parts.len() == 1 {
    return message.to_lowercase().contains(&pattern.to_lowercase());
  }

  let mut pos = 0;
  for (i, part) in parts.iter().enumerate() {
    if part.is_empty() {
      continue;
    }

    if i == 0 {
      if !message[pos..].starts_with(part) {
        return false;
      }
      pos += part.len();
    } else if i == parts.len() - 1 {
      if !message[pos..].ends_with(part) {
        return false;
      }
    } else if let Some(found_pos) = message[pos..].find(part) {
      pos += found_pos + part.len();
    } else {
      return false;
    }
  }

  true
}
