pub(crate) use crate::llvm::blocks::generator::BlockContext;
use boron_ir::IrBlock;
use inkwell::values::FunctionValue;

mod generator;

pub struct BlockGeneratorContext<'ctx, 'block> {
  pub block: &'block IrBlock,
  pub function: FunctionValue<'ctx>,
  pub context: BlockContext,
}

impl<'ctx, 'block> BlockGeneratorContext<'ctx, 'block> {
  pub fn new(
    block: &'block IrBlock,
    function: FunctionValue<'ctx>,
    context: BlockContext,
  ) -> Self {
    Self { block, function, context }
  }
}
