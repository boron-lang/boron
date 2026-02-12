use crate::directives::Directive;
use crate::output::{FailureType, TestResult, TestStatus};
use crate::test::Test;
use boron_core::prelude::*;
use boron_diagnostics::DiagnosticWriter;
use itertools::Itertools;
use std::panic::{AssertUnwindSafe, catch_unwind};

use super::directives::matches_directive;
use super::panic::{
  PanicHookGuard, PanicRunGuard, clear_last_backtrace, install_panic_hook, panic_message,
  take_last_backtrace,
};

pub fn run_single_test_in_process(test: &Test) -> TestResult {
  install_panic_hook();
  clear_last_backtrace();
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

  let output = DiagnosticWriter::buffer();
  let result = catch_unwind(AssertUnwindSafe(|| {
    let sess = Session::new(
      ProjectConfig {
        entrypoint: test.path.clone(),
        package_type: project_type,
        packages: vec![],
        mode: Mode::Debug,
        name: format!("test-{}", test.id.0),
        lib_type: LibType::Static,
        output: Default::default(),
        root: test.path.parent().unwrap().to_path_buf(),
        compiler: None,
        diagnostic_output_type: DiagnosticOutputType::HumanReadable,
        color: true,
        check_only: true,
        verbose: false,
        no_backtrace: false,
        timings: false,
      },
      output.clone(),
      CompilationMode::TestRunner,
    );

    let result = compiler_entrypoint(&sess);

    if let Ok(_) = result {
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

  let result = result.unwrap_or_else(|panic_payload| {
    let message = panic_message(panic_payload);
    if let Some(backtrace) = take_last_backtrace() {
      TestStatus::Panicked(format!("{message}\nbacktrace:\n{backtrace}"))
    } else {
      TestStatus::Panicked(message)
    }
  });
  let output_bytes = output.buffer_bytes().unwrap_or_default();
  TestResult { result, output: output_bytes, test_id: test.id }
}
