mod app;
mod directives;
mod output;
mod runner;
mod test;

use crate::app::{App, AppState};
use crate::directives::LineDirection;
use crate::output::{FailureType, TestResultPayload, TestStatus};
use crate::runner::{TestRunner, run_single_test_in_process};
use crate::test::Test;
use boron_core::prelude::{Colorize, canonicalize_with_strip};
use boron_core::vars::FILE_EXTENSION;
use color_eyre::owo_colors::OwoColorize;
use glob::glob;
use spinners::{Spinner, Spinners};
use std::env;
use std::io::{Write, stderr, stdout};
use std::path::{Path, PathBuf};
use std::process::Command;
use std::thread::Builder;
use std::time::Instant;

fn main() -> color_eyre::Result<()> {
  color_eyre::install()?;

  if let Some(single_test) = parse_single_test_arg() {
    return run_single_test_mode(single_test);
  }

  build_compiler();

  // we don't want to count the compiler building
  let instant = Instant::now();
  let args: Vec<PathBuf> = env::args_os().skip(1).map(PathBuf::from).collect();
  let tests = collect_tests(&args)?;

  let state = AppState::new(tests.len() as u64);
  let app = App::new(state.clone());
  let ui_handle = Builder::new()
    .name("UI thread for test runner".to_string())
    .spawn(move || app.run())?;

  let mut runner = TestRunner::new(&tests, state.clone());
  runner.run_with_progress()?;

  ui_handle.join().unwrap()?;

  let mut passed = 0;
  let mut failed = 0;
  let mut panicked = 0;
  let mut skipped = 0;
  let results = state.lock();

  for result in &results.test_results {
    let test = tests.iter().find(|t| t.id == result.test_id).unwrap();

    match &result.result {
      TestStatus::Skipped => {
        skipped += 1;
      }
      TestStatus::Passed => passed += 1,
      TestStatus::Panicked(message) => {
        panicked += 1;
        println!(
          "\n\n{}{}{}:",
          "[".dimmed(),
          test.short_path.display().to_string().bright_red(),
          "]".dimmed()
        );
        println!("test panicked: {message}");
        stderr().write_all(&result.output)?;
      }
      TestStatus::Failed(failures) => {
        println!(
          "\n\n{}{}{}:",
          "[".dimmed(),
          test.short_path.display().to_string().bright_red(),
          "]".dimmed()
        );

        for failure in failures {
          match failure {
            FailureType::ExpectedErrorNotFound { line, pattern, direction } => {
              println!(
                "expected error on line {} with pattern: {pattern}, but none was found",
                match direction {
                  LineDirection::Up => line.saturating_sub(1) + 1,
                  LineDirection::Down => line + 2,
                }
              )
            }
            FailureType::ExpectedErrorsButCompiled => {
              println!("expected some errors but the test compiled")
            }
            FailureType::OtherCompilerError => {
              println!("compiler failed due to some other reasons")
            }
            FailureType::UnexpectedErrors(errors) => {
              println!(
                "found unexpected errors that weren't annotated with a directive:"
              );

              for error in errors {
                println!("  - {error}");
              }
            }
          }

          failed += 1;
        }

        stderr().write_all(&result.output)?;
      }
    }
  }

  println!(
    "\n\n Summary: {} | {} | {} | {} finished in {}",
    format!("{} passed", passed).bright_green(),
    format!("{} failed", failed).bright_red(),
    format!("{} panicked", panicked).bright_magenta(),
    format!("{} skipped", skipped).bright_yellow(),
    format!("{:.2?}", instant.elapsed()).bright_magenta()
  );

  Ok(())
}

fn run_single_test_mode(path: PathBuf) -> color_eyre::Result<()> {
  let test = Test::try_new(canonicalize_with_strip(path)?)?;
  let result = run_single_test_in_process(&test);
  let payload = TestResultPayload {
    result: result.result,
    output: String::from_utf8_lossy(&result.output).to_string(),
  };
  let json = serde_json::to_vec(&payload)?;
  stdout().write_all(&json)?;
  Ok(())
}

fn parse_single_test_arg() -> Option<PathBuf> {
  let mut args = env::args_os().skip(1);

  while let Some(arg) = args.next() {
    if arg == "--single" {
      return args.next().map(PathBuf::from);
    }
  }

  None
}

fn build_compiler() {
  let mut spinner = Spinner::new(Spinners::Aesthetic, "Building compiler".to_string());

  let output = Command::new("cargo").arg("build").arg("-p").arg("boron").output();

  match output {
    Ok(output) if output.status.success() => {
      spinner.stop_with_message("Finished building compiler".bright_green().to_string());
    }
    Ok(output) => {
      spinner.stop_with_message("Failed to build the compiler".bright_red().to_string());
      eprintln!("{}", String::from_utf8_lossy(&output.stderr));
    }
    Err(e) => {
      spinner.stop_with_message("Failed to build the compiler".bright_red().to_string());
      eprintln!("Error running cargo: {}", e);
    }
  }
}

fn collect_tests(paths: &[PathBuf]) -> color_eyre::Result<Vec<Test>> {
  let test_files = find_test_files(paths)?;
  test_files
    .into_iter()
    .map(|path| Test::try_new(canonicalize_with_strip(path)?))
    .collect()
}

fn find_test_files(paths: &[PathBuf]) -> color_eyre::Result<Vec<PathBuf>> {
  let mut files = Vec::new();

  for path in paths {
    if path.is_file() {
      files.push(path.clone());
    } else {
      files.extend(find_files_in_directory(path)?);
    }
  }

  Ok(files)
}

fn find_files_in_directory(dir: &Path) -> color_eyre::Result<Vec<PathBuf>> {
  let pattern = format!("{}/**/*.{}", dir.display(), FILE_EXTENSION);

  let files = glob(&pattern)?
    .filter_map(|entry| match entry {
      Ok(path) => Some(path),
      Err(e) => {
        eprintln!("Error reading path: {:?}", e);
        None
      }
    })
    .collect();

  Ok(files)
}
