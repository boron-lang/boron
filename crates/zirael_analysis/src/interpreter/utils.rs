#[derive(Debug)]
pub struct InterpreterLimits {
  pub max_steps: u64,
  pub max_stack_depth: usize,
  pub max_allocs: usize,
  pub max_elements: usize,
}

impl Default for InterpreterLimits {
  fn default() -> Self {
    Self {
      max_steps: 100_000,
      max_stack_depth: 128,
      max_allocs: 1_024,
      max_elements: 100_000,
    }
  }
}
