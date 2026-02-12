use anyhow::Result;
use boron_core::prelude::{strip_same_root, Session};
use std::env::current_dir;
use std::io::{stderr, Write};
use std::panic::PanicHookInfo;
use std::path::PathBuf;
use sysinfo::System;
use yansi::Paint;

const SYSTEM_PATH_PATTERNS: &[&str] = &[
  "/std/",
  "\\std\\",
  "/core/",
  "\\core\\",
  "/alloc/",
  "\\alloc\\",
  "\\vcstartup\\",
  "/vcstartup/",
  "backtrace-",
  "\\backtrace\\",
  "/backtrace/",
];

const SYSTEM_FUNCTION_PATTERNS: &[&str] = &[
  "::std::",
  "::core::",
  "::alloc::",
  "std::",
  "core::",
  "alloc::",
  "backtrace::",
  "::backtrace::",
  "::_",
  "BaseThreadInitThunk",
  "RtlUserThreadStart",
  "invoke_main",
  "__scrt_common_main",
  "call_once",
];

pub fn setup_panic_handler(sess: &Session) {
  let no_backtrace = sess.config.no_backtrace;
  let triple = sess.target().triple();
  std::panic::set_hook(Box::new(move |info| {
    let buf = &mut Vec::new();
    let message = extract_panic_message(info);
    let location = extract_location(info);

    let _ = writeln!(buf, "");
    let _ = writeln!(buf, "  {} {}", "panic:".bright_red(), message);
    print_system_info(&location, &triple, buf);

    if !no_backtrace {
      let mut frames = Vec::new();
      backtrace::trace(|frame| {
        frames.push(frame.clone());
        true
      });
      let formatted = format_backtrace_frames(frames);

      let _ = write!(buf, "{}", formatted);
    }

    let _ = writeln!(buf, "");
    let _ = stderr().write_all(buf);
  }));
}

fn extract_panic_message(info: &PanicHookInfo<'_>) -> String {
  info
    .payload()
    .downcast_ref::<&str>()
    .map(|s| (*s).to_string())
    .or_else(|| info.payload().downcast_ref::<String>().cloned())
    .unwrap_or_else(|| "unknown error".to_string())
}

fn extract_location(info: &PanicHookInfo<'_>) -> String {
  info
    .location()
    .map(|loc| format!("{}:{}", loc.file(), loc.line()))
    .unwrap_or_default()
    .replace('\\', "/")
}

fn print_system_info(location: &str, triple: &str, buf: &mut Vec<u8>) {
  let mut sys = System::new_all();
  sys.refresh_all();

  let _ = writeln!(buf, "");
  let _ = writeln!(
    buf,
    "  {} {}",
    "location".dim(),
    if location.is_empty() {
      "<unknown>".dim()
    } else {
      location.bright_cyan().underline()
    }
  );
  let _ = writeln!(buf, "  {} {}", "triple".dim(), triple.bright_cyan().underline());
  let _ = writeln!(
    buf,
    "  {} {}",
    "please report at:".dim(),
    "https://github.com/boron-lang/boron".bright_cyan().underline()
  );
  let _ = writeln!(buf, "");
}

fn format_backtrace_frames(frames: Vec<backtrace::Frame>) -> String {
  let mut output = String::new();
  let mut consecutive_system_lines = 0;
  let mut last_system_frame = String::new();

  for frame in frames {
    backtrace::resolve_frame(&frame, |symbol| {
      let (frame_text, is_system) = format_frame_symbol(symbol);

      if is_system {
        consecutive_system_lines += 1;
        last_system_frame = frame_text;
      } else {
        if consecutive_system_lines > 0 {
          append_system_frames_summary(
            &mut output,
            consecutive_system_lines,
            &last_system_frame,
          );
          consecutive_system_lines = 0;
        }
        output.push_str(&format!("  {} {}\n", "→".bright_green(), frame_text));
      }
    });
  }

  if consecutive_system_lines > 0 {
    append_system_frames_summary(
      &mut output,
      consecutive_system_lines,
      &last_system_frame,
    );
  }

  output
}

fn format_frame_symbol(symbol: &backtrace::Symbol) -> (String, bool) {
  let mut is_system = false;

  let name_part = symbol.name().map_or_else(
    || "at <unknown>".dim().to_string(),
    |name| {
      let name_str = name.to_string();
      is_system = is_system_function(&name_str);
      format!("{} {}", "at".bright_cyan(), name_str.dim())
    },
  );

  let mut frame_text = name_part;

  if let Some(filename) = symbol.filename() {
    let file_path = filename.to_str().unwrap_or("");

    if !is_system {
      is_system = is_system_path(file_path);
    }

    if file_path.contains("core/src/ops/function.rs") {
      is_system = false;
    }

    let line = symbol.lineno().unwrap_or(0);
    let col = symbol.colno().unwrap_or(0);

    let shortened_path =
      shorten_path(file_path).unwrap_or_else(|_| file_path.to_string());

    frame_text = format!(
      "{}: {}",
      frame_text,
      format!("({}:{}:{})", shortened_path, line, col).bright_cyan()
    );
  }

  (frame_text, is_system)
}

fn is_system_function(name: &str) -> bool {
  SYSTEM_FUNCTION_PATTERNS.iter().any(|pattern| name.contains(pattern))
    || (name == "main") && !name.starts_with("core::ops::function")
}

fn is_system_path(path: &str) -> bool {
  SYSTEM_PATH_PATTERNS.iter().any(|pattern| path.contains(pattern))
}

fn append_system_frames_summary(output: &mut String, count: usize, last_frame: &str) {
  if count == 1 {
    output.push_str(&format!("  {}\n", last_frame));
  } else {
    let collapse_msg = format!("... collapsed {} lines from system code ...", count);
    output.push_str(&format!("  {}\n", collapse_msg.bright_magenta().italic()));
  }
}

pub fn shorten_path(path: &str) -> Result<String> {
  let path = PathBuf::from(path);
  let should_skip = path.starts_with("/rustc/") || path.starts_with("\\rustc\\");

  let shortened = path.iter().skip(if should_skip { 3 } else { 0 }).collect::<PathBuf>();

  Ok(strip_same_root(&shortened, &current_dir()?).to_string_lossy().to_string())
}
