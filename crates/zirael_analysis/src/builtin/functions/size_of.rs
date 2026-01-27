use crate::InferTy;
use crate::expander::BuiltInExpander;
use crate::interpreter::values::ConstValue;

impl<'a> BuiltInExpander<'a> {
  pub fn size_of(&self, ty: InferTy) -> ConstValue {
    let target = self.ctx.session.target();

    let x = match ty {
      InferTy::Primitive(p, _) => ConstValue::Int(target.size_of(p) as i128),
      _ => ConstValue::Int(0),
    };
    println!("{:?}", x);
    x
  }
}
