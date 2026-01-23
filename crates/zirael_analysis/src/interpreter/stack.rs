use crate::interpreter::values::ConstValue;
use dashmap::DashMap;
use zirael_hir::LocalId;

#[derive(Debug)]
pub struct Frame {
  locals: DashMap<LocalId, ConstValue>,
}

impl Frame {
  pub fn new() -> Self {
    Self {
      locals: DashMap::new(),
    }
  }

  pub fn insert_local(&self, id: LocalId, val: ConstValue) {
    self.locals.insert(id, val);
  }

  pub fn get_local(&self, id: &LocalId) -> Option<ConstValue> {
    self.locals.get(id).map(|v| v.clone())
  }
}

#[derive(Debug)]
pub struct Stack {
  frames: Vec<Frame>,
}

impl Stack {
  pub fn new() -> Self {
    Self { frames: vec![] }
  }

  pub fn push_frame(&mut self) {
    self.frames.push(Frame::new());
  }

  pub fn pop_frame(&mut self) -> Option<Frame> {
    self.frames.pop()
  }

  pub fn current_frame(&self) -> Option<&Frame> {
    self.frames.last()
  }

  pub fn current_frame_mut(&mut self) -> Option<&mut Frame> {
    self.frames.last_mut()
  }

  pub fn insert_local(&self, id: LocalId, val: ConstValue) {
    match self.current_frame() {
      Some(frame) => {
        frame.insert_local(id, val);
      }
      None => panic!("No active frame"),
    }
  }

  pub fn get_local(&self, id: &LocalId) -> Option<ConstValue> {
    for frame in self.frames.iter().rev() {
      if let Some(val) = frame.get_local(id) {
        return Some(val);
      }
    }
    None
  }
}
