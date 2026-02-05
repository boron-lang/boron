use crate::output::{TestResult, TestResultPayload};
use crate::test::Test;
use std::env;
use std::process::Command;

use super::panic::panic_message_from_stderr;

pub(crate) fn run_single_test_subprocess(test: &Test) -> Result<TestResult, (String, Vec<u8>)> {
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
