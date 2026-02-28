use crate::macro_impl::MacroFunctionError;
use crate::utils::is_valid_format_ident;
use std::collections::BTreeSet;

pub fn extract_named_placeholders(fmt: &str) -> Result<Vec<String>, MacroFunctionError> {
  let mut out: BTreeSet<String> = BTreeSet::new();
  let mut chars = fmt.chars().peekable();

  while let Some(ch) = chars.next() {
    match ch {
      '{' => {
        if chars.peek() == Some(&'{') {
          chars.next();
          continue;
        }

        let mut inner = String::new();
        let mut closed = false;
        for next in chars.by_ref() {
          if next == '}' {
            closed = true;
            break;
          }
          inner.push(next);
        }

        if !closed {
          return Err(MacroFunctionError::InvalidAttribute(
            "unclosed `{` in format string".to_string(),
          ));
        }

        let inner = inner.trim();
        if inner.is_empty() {
          return Err(MacroFunctionError::InvalidAttribute(
            "positional formatting (`{}`) is not supported; use `{field}`".to_string(),
          ));
        }

        // Split off formatting spec (e.g. name:? or name:>10)
        let name_part = inner.split([':', '!']).next().unwrap_or("").trim();

        if name_part.is_empty() {
          return Err(MacroFunctionError::InvalidAttribute(
            "positional formatting (`{:...}`) is not supported; use `{field}`"
              .to_string(),
          ));
        }

        if name_part.chars().next().is_some_and(|c| c.is_ascii_digit()) {
          return Err(MacroFunctionError::InvalidAttribute(
            "positional formatting (`{0}`) is not supported; use `{field}`".to_string(),
          ));
        }

        if !is_valid_format_ident(name_part) {
          return Err(MacroFunctionError::InvalidAttribute(format!(
            "invalid format placeholder `{{{}}}`; use a field name like `{{expected}}`",
            name_part
          )));
        }

        out.insert(name_part.to_string());
      }
      '}' => {
        if chars.peek() == Some(&'}') {
          chars.next();
        } else {
          return Err(MacroFunctionError::InvalidAttribute(
            "unmatched `}` in format string".to_string(),
          ));
        }
      }
      _ => {}
    }
  }

  Ok(out.into_iter().collect())
}
