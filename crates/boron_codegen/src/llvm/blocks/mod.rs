pub(crate) use crate::llvm::blocks::generator::BlockContext;
use boron_ir::{IrBlock, IrFunction};
use inkwell::values::FunctionValue;

mod generator;

pub struct BlockGeneratorContext<'ctx, 'block> {
  pub block: &'block IrBlock,
  pub ir_function: &'block IrFunction,
  pub function: FunctionValue<'ctx>,
  pub context: BlockContext,
}

impl<'ctx, 'block> BlockGeneratorContext<'ctx, 'block> {
  pub fn new(
    block: &'block IrBlock,
    ir_function: &'block IrFunction,
    function: FunctionValue<'ctx>,
    context: BlockContext,
  ) -> Self {
    Self { block, ir_function, function, context }
  }
}
