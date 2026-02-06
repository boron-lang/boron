use std::fmt::Display;
use yansi::{Color, Paint, Painted};

pub trait Fmt: Sized {
  fn fg<C: Into<Option<Color>>>(self, color: C) -> Painted<Self>
  where
    Self: Display,
  {
    let mut painted = Paint::new(self);
    if let Some(col) = color.into() {
      painted = painted.fg(col);
    }
    painted
  }

  fn bold(self) -> Painted<Self>
  where
    Self: Display,
  {
    Paint::new(self).bold()
  }

  fn italic(self) -> Painted<Self>
  where
    Self: Display,
  {
    Paint::new(self).italic()
  }
}

impl<T: Display> Fmt for T {}
