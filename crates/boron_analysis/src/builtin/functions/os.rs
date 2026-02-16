use crate::expander::BuiltInExpander;
use crate::interpreter::values::ConstValue;

impl<'ctx> BuiltInExpander<'ctx> {
    pub fn os_builtin(&self) -> ConstValue {
        ConstValue::String("hello".to_string())
    }
}