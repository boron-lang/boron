use crate::directives::LineDirection;
use crate::test::TestId;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TestStatus {
  Passed,
  Failed(Vec<FailureType>),
  Panicked(String),
  Skipped,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum FailureType {
  OtherCompilerError,
  ExpectedErrorNotFound { line: usize, pattern: String, direction: LineDirection },
  ExpectedErrorsButCompiled,
  UnexpectedErrors(Vec<String>),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TestResultPayload {
  pub result: TestStatus,
  pub output: String,
}

#[derive(Debug, Clone)]
pub struct TestResult {
  pub result: TestStatus,
  pub output: Vec<u8>,
  pub test_id: TestId,
}
