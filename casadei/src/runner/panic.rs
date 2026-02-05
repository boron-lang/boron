use std::any::Any;
use std::cell::Cell;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Once;

thread_local! {
  static SUPPRESS_PANIC_HOOK: Cell<bool> = const { Cell::new(false) };
}

static RUNNING_TESTS: AtomicBool = AtomicBool::new(false);
static PANIC_HOOK_INSTALLED: Once = Once::new();

pub(crate) fn install_panic_hook() {
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

pub(crate) struct PanicRunGuard;

impl PanicRunGuard {
  pub(crate) fn new() -> Self {
    RUNNING_TESTS.store(true, Ordering::Relaxed);
    Self
  }
}

impl Drop for PanicRunGuard {
  fn drop(&mut self) {
    RUNNING_TESTS.store(false, Ordering::Relaxed);
  }
}

pub(crate) struct PanicHookGuard;

impl PanicHookGuard {
  pub(crate) fn new() -> Self {
    SUPPRESS_PANIC_HOOK.with(|flag| flag.set(true));
    Self
  }
}

impl Drop for PanicHookGuard {
  fn drop(&mut self) {
    SUPPRESS_PANIC_HOOK.with(|flag| flag.set(false));
  }
}

pub(crate) fn panic_message(panic_payload: Box<dyn Any + Send>) -> String {
  if let Some(message) = panic_payload.downcast_ref::<&str>() {
    (*message).to_string()
  } else if let Some(message) = panic_payload.downcast_ref::<String>() {
    message.clone()
  } else {
    "unknown panic".to_string()
  }
}

pub(crate) fn panic_message_from_stderr(stderr: &[u8], status: String) -> String {
  let message = String::from_utf8_lossy(stderr).trim().to_string();

  if message.is_empty() { format!("child process crashed ({status})") } else { message }
}
