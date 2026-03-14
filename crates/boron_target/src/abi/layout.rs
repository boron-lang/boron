#[derive(Clone, Debug, serde::Serialize, serde::Deserialize)]
pub struct Layout {
  pub size: usize,
  pub alignment: Alignment,
}

impl Layout {
  pub fn new(size: usize, alignment: usize) -> Self {
    let alignment = Alignment::new(alignment);
    let size = align_up(size, alignment.get());
    Self { size, alignment }
  }
}

#[derive(Clone, Debug, serde::Serialize, serde::Deserialize)]
pub struct Alignment(usize);
impl Alignment {
  pub fn new(alignment: usize) -> Self {
    assert_ne!(alignment, 0);
    assert!(alignment.is_power_of_two());
    Self(alignment)
  }

  pub fn get(&self) -> usize {
    self.0
  }
}

pub fn align_up(size: usize, align: usize) -> usize {
  (size + align - 1) & !(align - 1)
}

impl From<usize> for Alignment {
  fn from(value: usize) -> Self {
    Self::new(value)
  }
}

impl Default for Alignment {
  fn default() -> Self {
    Self::new(1)
  }
}
