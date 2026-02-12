use crate::InferTy;
use crate::expander::BuiltInExpander;
use crate::interpreter::values::ConstValue;
use crate::size_of::{SizeOfContext, size_of_ty};

impl BuiltInExpander<'_> {
  pub fn size_of(&self, ty: &InferTy) -> ConstValue {
    let ctx = SizeOfContext { sess: self.sess, hir: self.hir, resolver: self.resolver };

    ConstValue::Int(size_of_ty(&ctx, ty) as i128)
  }
}
