use crate::app::AppState;
use crate::directives::{Directive, LineDirection};
use crate::output::{FailureType, TestResult, TestResultPayload, TestStatus};
use crate::test::Test;
use boron_core::prelude::*;
use itertools::Itertools;
use parking_lot::Mutex;
use rayon::prelude::*;
use std::any::Any;
use std::cell::Cell;
use std::env;
use std::io::Cursor;
use std::panic::{catch_unwind, AssertUnwindSafe};
use std::process::Command;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::sync::Once;

pub struct TestRunner<'tests> {
  pub tests: &'tests Vec<Test>,
  pub state: Arc<Mutex<AppState>>,
}

impl<'tests> TestRunner<'tests> {
  pub fn new(tests: &'tests Vec<Test>, state: Arc<Mutex<AppState>>) -> Self {
    Self { tests, state }
  }

  pub fn run_with_progress(&mut self) -> color_eyre::Result<()> {
    self.tests.par_iter().for_each(|test| {
      let result = self.run_single_test(test);

      let lock = &mut self.state.lock();
      lock.increment_completed();
      lock.add_result(result);
    });

    Ok(())
  }

  fn run_single_test(&'tests self, test: &'tests Test) -> TestResult {
    run_single_test_subprocess(test).unwrap_or_else(|(message, output)| TestResult {
      result: TestStatus::Panicked(message),
      output,
      test_id: test.id,
    })
  }
}

pub fn run_single_test_in_process(test: &Test) -> TestResult {
  install_panic_hook();
  let _panic_guard = PanicRunGuard::new();
  let _panic_thread_guard = PanicHookGuard::new();
  let project_type = test
    .directives
    .iter()
    .find_map(|directive| match directive {
      Directive::PackageType(package_type) => Some(*package_type),
      _ => None,
    })
    .unwrap_or(PackageType::Binary);

  let output = Arc::new(Mutex::new(Cursor::new(vec![])));
  let result = catch_unwind(AssertUnwindSafe(|| {
    let result = compiler_entrypoint(
      &ProjectConfig {
        entrypoint: test.path.clone(),
        project_type,
        packages: vec![],
        mode: Mode::Debug,
        name: format!("test-{}", test.id.0),
        lib_type: LibType::Static,
        output: Default::default(),
        root: test.path.parent().unwrap().to_path_buf(),
        diagnostic_output_type: DiagnosticOutputType::HumanReadable,
        color: true,
      },
      output.clone(),
      true,
      true,
    );

    if let Ok(sess) = result {
      let sources = sess.dcx().sources();

      if sess.dcx().has_errors() {
        let mut failures = vec![];

        let error_directives = test
          .directives
          .iter()
          .filter_map(|d| match d {
            Directive::Error { line, direction, pattern } => {
              Some((*line, direction.clone(), pattern.clone()))
            }
            _ => None,
          })
          .collect_vec();

        failures.extend(
          error_directives
            .iter()
            .filter(|(directive_line, direction, pattern)| {
              !sess.dcx().diagnostics.iter().any(|diagnostic| {
                matches_directive(
                  diagnostic.value(),
                  *directive_line,
                  direction,
                  pattern,
                  sources,
                )
              })
            })
            .map(|(line, direction, pattern)| FailureType::ExpectedErrorNotFound {
              line: *line,
              direction: direction.clone(),
              pattern: pattern.clone(),
            }),
        );

        let unexpected_errors: Vec<String> = sess
          .dcx()
          .diagnostics
          .iter()
          .filter(|diagnostic| {
            !error_directives.iter().any(|(directive_line, direction, pattern)| {
              matches_directive(
                diagnostic.value(),
                *directive_line,
                direction,
                pattern,
                sources,
              )
            })
          })
          .map(|diagnostic| diagnostic.value().diag.message.clone())
          .collect();

        if !unexpected_errors.is_empty() {
          failures.push(FailureType::UnexpectedErrors(unexpected_errors));
        }

        if failures.is_empty() {
          TestStatus::Passed
        } else {
          TestStatus::Failed(failures)
        }
      } else {
        let has_error_directives =
          test.directives.iter().any(|d| matches!(d, Directive::Error { .. }));

        if has_error_directives {
          TestStatus::Failed(vec![FailureType::ExpectedErrorsButCompiled])
        } else {
          TestStatus::Passed
        }
      }
    } else {
      TestStatus::Failed(vec![FailureType::OtherCompilerError])
    }
  }));

  let result = match result {
    Ok(status) => status,
    Err(panic_payload) => TestStatus::Panicked(panic_message(panic_payload)),
  };

  TestResult { result, output: output.lock().get_ref().clone(), test_id: test.id }
}

fn run_single_test_subprocess(test: &Test) -> Result<TestResult, (String, Vec<u8>)> {
  let exe = env::current_exe()
    .map_err(|error| (format!("failed to locate casadei executable: {error}"), vec![]))?;

  let output = Command::new(exe)
    .arg("--single")
    .arg(&test.path)
    .env("CASADEI_CHILD", "1")
    .output()
    .map_err(|error| (format!("failed to spawn casadei child: {error}"), vec![]))?;

  if !output.status.success() {
    let message = panic_message_from_stderr(&output.stderr, output.status.to_string());
    return Err((message, output.stderr));
  }

  let payload: TestResultPayload = serde_json::from_slice(&output.stdout)
    .map_err(|error| (format!("failed to parse child result: {error}"), output.stdout))?;

  Ok(TestResult {
    result: payload.result,
    output: payload.output.into_bytes(),
    test_id: test.id,
  })
}

thread_local! {
  static SUPPRESS_PANIC_HOOK: Cell<bool> = const { Cell::new(false) };
}

static RUNNING_TESTS: AtomicBool = AtomicBool::new(false);
static PANIC_HOOK_INSTALLED: Once = Once::new();

fn install_panic_hook() {
  PANIC_HOOK_INSTALLED.call_once(|| {
    let default_hook = std::panic::take_hook();
    std::panic::set_hook(Box::new(move |info| {
      let suppress = RUNNING_TESTS.load(Ordering::Relaxed)
        || SUPPRESS_PANIC_HOOK.with(|flag| flag.get());

      if !suppress {
        default_hook(info);
      }
    }));
  });
}

struct PanicRunGuard;

impl PanicRunGuard {
  fn new() -> Self {
    RUNNING_TESTS.store(true, Ordering::Relaxed);
    Self
  }
}

impl Drop for PanicRunGuard {
  fn drop(&mut self) {
    RUNNING_TESTS.store(false, Ordering::Relaxed);
  }
}

struct PanicHookGuard;

impl PanicHookGuard {
  fn new() -> Self {
    SUPPRESS_PANIC_HOOK.with(|flag| flag.set(true));
    Self
  }
}

impl Drop for PanicHookGuard {
  fn drop(&mut self) {
    SUPPRESS_PANIC_HOOK.with(|flag| flag.set(false));
  }
}

fn panic_message(panic_payload: Box<dyn Any + Send>) -> String {
  if let Some(message) = panic_payload.downcast_ref::<&str>() {
    (*message).to_string()
  } else if let Some(message) = panic_payload.downcast_ref::<String>() {
    message.clone()
  } else {
    "unknown panic".to_string()
  }
}

fn panic_message_from_stderr(stderr: &[u8], status: String) -> String {
  let message = String::from_utf8_lossy(stderr).trim().to_string();

  if message.is_empty() { format!("child process crashed ({status})") } else { message }
}

fn matches_directive(
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
