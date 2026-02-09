use parking_lot::{Mutex, MutexGuard};
use std::io::{Cursor, Stderr, Write, stderr};
use std::sync::Arc;

pub enum DiagnosticWriterInner {
  Stderr(Stderr),
  Buffer(Cursor<Vec<u8>>),
  Custom(Box<dyn Write + Send>),
}

impl Write for DiagnosticWriterInner {
  fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
    match self {
      Self::Stderr(writer) => writer.write(buf),
      Self::Buffer(writer) => writer.write(buf),
      Self::Custom(writer) => writer.write(buf),
    }
  }

  fn flush(&mut self) -> std::io::Result<()> {
    match self {
      Self::Stderr(writer) => writer.flush(),
      Self::Buffer(writer) => writer.flush(),
      Self::Custom(writer) => writer.flush(),
    }
  }
}

#[derive(Clone)]
pub struct DiagnosticWriter {
  inner: Arc<Mutex<DiagnosticWriterInner>>,
}

impl DiagnosticWriter {
  pub fn stderr() -> Self {
    Self { inner: Arc::new(Mutex::new(DiagnosticWriterInner::Stderr(stderr()))) }
  }

  pub fn buffer() -> Self {
    Self {
      inner: Arc::new(Mutex::new(DiagnosticWriterInner::Buffer(Cursor::new(Vec::new())))),
    }
  }

  pub fn custom(writer: Box<dyn Write + Send>) -> Self {
    Self { inner: Arc::new(Mutex::new(DiagnosticWriterInner::Custom(writer))) }
  }

  pub fn lock(&self) -> MutexGuard<'_, DiagnosticWriterInner> {
    self.inner.lock()
  }

  pub fn buffer_bytes(&self) -> Option<Vec<u8>> {
    let guard = self.inner.lock();
    match &*guard {
      DiagnosticWriterInner::Buffer(writer) => Some(writer.get_ref().clone()),
      _ => None,
    }
  }

  pub fn flush(&self) {
    let mut guard = self.inner.lock();
    let _ = guard.flush();
  }
}
