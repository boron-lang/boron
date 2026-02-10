use crate::emitters::human_readable::HumanReadableEmitter;
use crate::emitters::Emitter;
use crate::output_type::DiagnosticOutputType;
use crate::writer::DiagnosticWriter;
use crate::{Diag, Diagnostic, DiagnosticId, DiagnosticLevel};
use boron_source::prelude::Sources;
use dashmap::mapref::one::{Ref, RefMut};
use dashmap::DashMap;
use derivative::Derivative;
use log::debug;
use std::io::{stderr, Write as _, Write as _};
use std::sync::Arc;

#[derive(Derivative)]
#[derivative(Debug)]
pub struct DiagnosticCtx {
  pub diagnostics: Arc<DashMap<DiagnosticId, Diagnostic>>,
  emitter: Box<dyn Emitter + Send + Sync>,
  #[derivative(Debug = "ignore")]
  pub writer: DiagnosticWriter,
  stop_on_error: bool,
}

pub trait ToDiagnostic {
  fn to_diagnostic(self) -> Diag;
}

impl DiagnosticCtx {
  pub fn new(
    sources: Arc<Sources>,
    color: bool,
    diagnostic_output_type: &DiagnosticOutputType,
    writer: DiagnosticWriter,
  ) -> Self {
    let emitter: Box<dyn Emitter + Send + Sync> = match diagnostic_output_type {
      DiagnosticOutputType::HumanReadable => {
        Box::new(HumanReadableEmitter::new(sources, color))
      }
      DiagnosticOutputType::Json => {
        unimplemented!("JSON output not yet implemented")
      }
    };
    Self { diagnostics: Default::default(), emitter, stop_on_error: true, writer }
  }

  pub fn sources(&self) -> &Sources {
    self.emitter.sources()
  }

  pub fn add(&self, diag: Diag) -> DiagnosticId {
    let id = DiagnosticId::new();
    self.diagnostics.insert(
      id,
      Diagnostic { id, diag: Box::new(diag), cancelled: false, emitted: false },
    );

    id
  }

  pub fn get(&self, id: DiagnosticId) -> Option<Ref<'_, DiagnosticId, Diagnostic>> {
    self.diagnostics.get(&id)
  }

  pub fn get_mut(
    &self,
    id: DiagnosticId,
  ) -> Option<RefMut<'_, DiagnosticId, Diagnostic>> {
    self.diagnostics.get_mut(&id)
  }

  pub fn cancel(&self, id: DiagnosticId) {
    if let Some(mut diag) = self.get_mut(id) {
      diag.cancelled = true;
    }
    debug!("cancelled diagnostic {id:?}");
  }

  pub fn emit(&self, diag: impl ToDiagnostic) {
    let diagnostic = diag.to_diagnostic();
    let level = diagnostic.level;
    let id = self.add(diagnostic);

    if level == DiagnosticLevel::Bug {
      self.emit_diag(id, &mut 0);
      self.flush_to_stderr();
      panic!("look at the emitted diagnostic")
    }
  }

  pub fn bug(&self, msg: impl Into<String>) {
    self.emit(Diagnostic {
      id: DiagnosticId::new(),
      diag: Box::new(Diag::new(msg.into(), DiagnosticLevel::Bug)),
      cancelled: false,
      emitted: false,
    });
  }

  // actually emits the diagnostic to stderr
  fn emit_diag(&self, id: DiagnosticId, collected: &mut i32) {
    let diagnostic = {
      let Some(diagnostic) = self.get(id) else {
        panic!("No diagnostic found for {id:?}");
      };

      if diagnostic.emitted || diagnostic.cancelled {
        return;
      }

      diagnostic.diag.clone()
    };

    let mut writer = self.writer.lock();
    self.emitter.emit_diagnostic(&diagnostic, &mut *writer).expect("TODO: panic message");

    if let Some(mut diag) = self.get_mut(id) {
      if diag.diag.level == DiagnosticLevel::Error {
        *collected += 1;
      }

      diag.emitted = true;
    }
  }

  pub fn emit_all(&self) -> i32 {
    let emitted = &mut 0;
    let ids: Vec<_> = self.diagnostics.iter().map(|d| d.id).collect();
    for id in ids {
      self.emit_diag(id, emitted);
    }

    *emitted
  }

  pub fn has_errors(&self) -> bool {
    self.diagnostics.iter().any(|diag| diag.diag.level == DiagnosticLevel::Error)
  }

  pub fn flush_to_stderr(&self) {
    if let Some(bytes) = self.writer.buffer_bytes() {
      let _ = stderr().write_all(&bytes);
      let _ = stderr().flush();
      return;
    }

    self.writer.flush();
  }
}
