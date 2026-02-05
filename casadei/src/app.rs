use crate::output::{TestResult, TestStatus};
use indicatif::{ProgressBar, ProgressStyle};
use parking_lot::Mutex;
use std::sync::Arc;
use std::time::Duration;

#[derive(Debug, Clone)]
pub struct AppState {
  pub total_tests: u64,
  pub completed_tests: u64,
  pub passed: u64,
  pub failed: u64,
  pub panicked: u64,
  pub skipped: u64,
  pub test_results: Vec<TestResult>,
}

impl AppState {
  pub fn new(total_tests: u64) -> Arc<Mutex<Self>> {
    Arc::new(Mutex::new(Self {
      total_tests,
      completed_tests: 0,
      passed: 0,
      failed: 0,
      panicked: 0,
      skipped: 0,
      test_results: vec![],
    }))
  }

  pub fn increment_completed(&mut self) {
    self.completed_tests += 1;
  }

  pub fn add_result(&mut self, result: TestResult) {
    match &result.result {
      TestStatus::Passed => self.passed += 1,
      TestStatus::Failed(_) => self.failed += 1,
      TestStatus::Panicked(_) => self.panicked += 1,
      TestStatus::Skipped => self.skipped += 1,
    }
    self.test_results.push(result);
  }
}

#[derive(Debug)]
pub struct App {
  overall: ProgressBar,
  state: Arc<Mutex<AppState>>,
}

impl App {
  pub fn new(state: Arc<Mutex<AppState>>) -> Self {
    let total_tests = state.lock().total_tests;

    let overall_style = ProgressStyle::with_template(
      "{spinner:.green} {msg:<24} {bar:40.cyan/blue} {pos:>4}/{len:<4} ({percent:>3}%)",
    )
    .unwrap()
    .progress_chars("█▉▊▋▌▍▎▏ ")
    .tick_chars("⠋⠙⠚⠞⠖⠦⠴⠲⠳⠓");

    let overall = ProgressBar::new(total_tests);
    overall.set_style(overall_style);
    overall.set_message("Overall");
    overall.enable_steady_tick(Duration::from_millis(100));

    Self { overall, state }
  }

  pub fn run(self) -> color_eyre::Result<()> {
    loop {
      let is_complete = {
        let state = self.state.lock();

        self.overall.set_length(state.total_tests.max(1));
        self.overall.set_position(state.completed_tests);
        self.overall.set_message(format!(
          "All tests {}/{} | passed {} | failed {} | panicked {} | skipped {}",
          state.completed_tests,
          state.total_tests,
          state.passed,
          state.failed,
          state.panicked,
          state.skipped
        ));

        state.total_tests == state.completed_tests
      };

      if is_complete {
        self.overall.finish_with_message("All tests done");
        break;
      }
    }

    Ok(())
  }
}
