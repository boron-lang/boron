use crate::expander::BuiltInExpander;
use crate::interpreter::values::ConstValue;

impl BuiltInExpander<'_> {
  pub fn os_builtin(&self) -> ConstValue {
    ConstValue::String("hello".to_owned())
  }
}
