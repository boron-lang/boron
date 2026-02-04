use crate::llvm::LLVMCodegen;
use crate::llvm::blocks::BlockGeneratorContext;
use boron_ir::{IrStmt, IrStmtKind, IrTerminator};

#[allow(dead_code)]
#[derive(Clone, Debug, PartialEq, Eq, Copy)]
pub enum BlockContext {
  FunctionStart,
  Normal,
  FunctionExit,
}

impl<'ctx> LLVMCodegen<'ctx> {
  fn block_name(ctx: &BlockGeneratorContext<'ctx, '_>) -> String {
    match ctx.context {
      BlockContext::FunctionStart => "start".to_owned(),
      BlockContext::FunctionExit => "exit".to_owned(),
      BlockContext::Normal => format!("bb{}", ctx.block.hir_id.local_id.0),
    }
  }

  pub fn generate_block(&self, ctx: &BlockGeneratorContext<'ctx, '_>) {
    let name = Self::block_name(&ctx);

    let entry = self.context.append_basic_block(ctx.function, &name);
    self.builder.position_at_end(entry);

    for stmt in &ctx.block.stmts {
      self.generate_stmt(stmt, ctx)
    }

    self.generate_terminator(&ctx.block.terminator);
  }

  pub fn generate_stmt(&self, stmt: &IrStmt, _ctx: &BlockGeneratorContext<'ctx, '_>) {
    match &stmt.kind {
      IrStmtKind::Local(local) => {
        let name = format!("local_{}", local.def_id.index());
        let _val = self.builder.build_alloca(self.ty(&local.ty), &name).expect("alloca");
      }
      _ => unimplemented!(),
    }
  }

  fn generate_terminator(&self, terminator: &IrTerminator) {
    match terminator {
      IrTerminator::Return(None) => {
        let _ = self.builder.build_return(None);
      }
      IrTerminator::Return(Some(_)) => {
        todo!("lower return value in LLVM codegen");
      }
      IrTerminator::Branch { .. } => {
        todo!("lower branch terminator in LLVM codegen");
      }
      IrTerminator::Goto { .. } => {
        todo!("lower goto terminator in LLVM codegen");
      }
      IrTerminator::Unreachable => {
        let _ = self.builder.build_unreachable();
      }
    }
  }
}
