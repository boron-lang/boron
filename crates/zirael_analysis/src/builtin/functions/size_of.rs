use crate::InferTy;
use crate::expander::BuiltInExpander;
use crate::interpreter::values::ConstValue;

impl<'a> BuiltInExpander<'a> {
  pub fn size_of(&self, ty: InferTy) -> ConstValue {
    match ty {
      InferTy::Primitive(kind, _) => {}
      _ => ConstValue::Int(0),
    }
  }
}
