use crate::queries::queries::Queries;

pub trait QueryProvider<'ctx> {
  fn provide(&self, queries: &mut Queries<'ctx>);
}