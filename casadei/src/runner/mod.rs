mod directives;
mod in_process;
mod panic;
mod subprocess;

use crate::app::AppState;
use crate::output::{TestResult, TestStatus};
use crate::test::Test;
use parking_lot::Mutex;
use rayon::prelude::*;
use std::sync::Arc;

pub use in_process::run_single_test_in_process;

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
    subprocess::run_single_test_subprocess(test).unwrap_or_else(|(message, output)| {
      TestResult { result: TestStatus::Panicked(message), output, test_id: test.id }
    })
  }
}
