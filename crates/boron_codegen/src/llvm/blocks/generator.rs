use crate::llvm::LLVMCodegen;
use crate::llvm::blocks::BlockGeneratorContext;
use anyhow::Result;
use boron_ir::{IrStmt, IrStmtKind, IrTerminator};

#[expect(dead_code)]
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

  pub fn generate_block(&self, ctx: &BlockGeneratorContext<'ctx, '_>) -> Result<()> {
    let name = Self::block_name(ctx);

    let entry = self.context.append_basic_block(ctx.function, &name);
    self.builder.position_at_end(entry);

    if ctx.context == BlockContext::FunctionStart {
      for (idx, param) in ctx.ir_function.params.iter().enumerate() {
        let param_val = self.require_some(
          ctx.function.get_nth_param(idx as u32),
          "param index should exist",
        )?;
        let alloca = self.builder.build_alloca(self.ty(&param.ty)?, &param.name);
        let alloca = self.require_llvm(alloca, "param alloca")?;

        let _ = self
          .require_llvm(self.builder.build_store(alloca, param_val), "param store")?;

        self.locals.insert(param.def_id, alloca);
      }

      self.generate_var_allocas(ctx.ir_function.id)?;
    }

    for stmt in &ctx.block.stmts {
      self.generate_stmt(stmt, ctx)?;
    }

    self.generate_terminator(&ctx.block.terminator)?;
    Ok(())
  }

  pub fn generate_stmt(
    &self,
    stmt: &IrStmt,
    _ctx: &BlockGeneratorContext<'ctx, '_>,
  ) -> Result<()> {
    match &stmt.kind {
      IrStmtKind::Expr(expr) => {
        self.generate_expr(expr)?;
      }
      IrStmtKind::Local(id) => {}
    }
    Ok(())
  }

  fn generate_terminator(&self, terminator: &IrTerminator) -> Result<()> {
    match terminator {
      IrTerminator::Return(None) => {
        let _ = self.builder.build_return(None);
      }
      IrTerminator::Return(Some(val)) => {
        let _ = self.builder.build_return(Some(&self.generate_expr(val)?));
      }
      IrTerminator::Branch { .. } => {
        todo!("lower branch terminator in LLVM codegen");
      }
      IrTerminator::Goto { target } => {
        todo!()
      }
      IrTerminator::Unreachable => {
        let _ = self.builder.build_unreachable();
      }
    }
    Ok(())
  }
}
