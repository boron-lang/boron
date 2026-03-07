use crate::directives::{Directive, LineDirection};
use crate::output::{FailureType, TestResult, TestStatus};
use crate::test::Test;
use boron_core::prelude::*;
use boron_diagnostics::{DiagnosticLevel, DiagnosticWriter};
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
    .find_map(|d| if let Directive::PackageType(pt) = d { Some(*pt) } else { None })
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
      let mut failures = vec![];

      let error_specs: Vec<(usize, LineDirection, String)> = test
        .directives
        .iter()
        .filter_map(|d| {
          if let Directive::Error { line, direction, pattern } = d {
            Some((*line, direction.clone(), pattern.clone()))
          } else {
            None
          }
        })
        .collect();

      let warning_specs: Vec<(usize, LineDirection, String)> = test
        .directives
        .iter()
        .filter_map(|d| {
          if let Directive::Warning { line, direction, pattern } = d {
            Some((*line, direction.clone(), pattern.clone()))
          } else {
            None
          }
        })
        .collect();

      let matches_spec =
        |diag: &Diagnostic, line: usize, dir: &LineDirection, pat: &str| {
          matches_directive(diag, line, dir, pat, sources)
        };
      let spec_matches_diag =
        |diag: &Diagnostic, specs: &[(usize, LineDirection, String)]| {
          specs.iter().any(|(line, dir, pat)| matches_spec(diag, *line, dir, pat))
        };

      if sess.dcx().has_errors() {
        failures.extend(
          error_specs
            .iter()
            .filter(|(line, dir, pat)| {
              !sess
                .dcx()
                .diagnostics
                .iter()
                .any(|d| matches_spec(d.value(), *line, dir, pat))
            })
            .map(|(line, dir, pat)| FailureType::ExpectedErrorNotFound {
              line: *line,
              direction: dir.clone(),
              pattern: pat.clone(),
            }),
        );

        let unexpected_errors: Vec<String> = sess
          .dcx()
          .diagnostics
          .iter()
          .filter(|d| {
            let diag = d.value();
            diag.diag.level == DiagnosticLevel::Error
              && !spec_matches_diag(diag, &error_specs)
          })
          .map(|d| d.value().diag.message.clone())
          .collect();

        if !unexpected_errors.is_empty() {
          failures.push(FailureType::UnexpectedErrors(unexpected_errors));
        }
      } else if !error_specs.is_empty() {
        failures.push(FailureType::ExpectedErrorsButCompiled);
      }

      let actual_warnings = sess
        .dcx()
        .diagnostics
        .iter()
        .filter(|d| d.value().diag.level == DiagnosticLevel::Warning)
        .collect_vec();

      failures.extend(
        warning_specs
          .iter()
          .filter(|(line, dir, pat)| {
            !actual_warnings.iter().any(|d| matches_spec(d.value(), *line, dir, pat))
          })
          .map(|(line, dir, pat)| FailureType::ExpectedWarningNotFound {
            line: *line,
            direction: dir.clone(),
            pattern: pat.clone(),
          }),
      );

      let unexpected_warnings: Vec<String> = actual_warnings
        .iter()
        .filter(|d| !spec_matches_diag(d.value(), &warning_specs))
        .map(|d| d.value().diag.message.clone())
        .collect();

      if !unexpected_warnings.is_empty() {
        failures.push(FailureType::UnexpectedWarnings(unexpected_warnings));
      }

      if failures.is_empty() { TestStatus::Passed } else { TestStatus::Failed(failures) }
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
