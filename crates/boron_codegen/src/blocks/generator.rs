use crate::blocks::BlockGeneratorContext;
use crate::codegen::LLVMCodegen;
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
  pub fn generate_block(&self, ctx: &BlockGeneratorContext<'ctx, '_>) -> Result<()> {
    let entry = self.require_some(
      self.blocks.get(&ctx.block.hir_id).map(|r| *r.value()),
      "block should have been pre-created",
    )?;
    self.builder.position_at_end(entry);

    if ctx.context == BlockContext::FunctionStart {
      for (idx, param) in ctx.ir_function.params.iter().enumerate() {
        let param_val = self.require_some(
          ctx.function.get_nth_param(idx as u32),
          "param index should exist",
        )?;
        let alloca = self
          .builder
          .build_alloca(self.ty(&param.ty)?, &format!("param.alloc.{}", param.name));
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

    self.generate_terminator(&ctx)?;
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
      IrStmtKind::Local(_) => {}
    }
    Ok(())
  }

  fn generate_terminator(&self, ctx: &BlockGeneratorContext<'ctx, '_>) -> Result<()> {
    match &ctx.block.terminator {
      IrTerminator::Return(None) => {
        let _ = self.builder.build_return(None);
      }
      IrTerminator::Return(Some(val)) => {
        self.require_llvm(
          self.builder.build_return(Some(&self.generate_expr(val)?)),
          "couldn't build return with value",
        )?;
      }
      IrTerminator::Branch { condition, then_target, else_target } => {
        let cond_value = self.generate_expr(condition)?.into_int_value();

        let then_block = self.require_some(
          self.blocks.get(then_target).map(|r| *r.value()),
          "then block should have been pre-created",
        )?;
        let else_block = self.require_some(
          self.blocks.get(else_target).map(|r| *r.value()),
          "else block should have been pre-created",
        )?;

        self.require_llvm(
          self.builder.build_conditional_branch(cond_value, then_block, else_block),
          "conditional branch",
        )?;
      }
      IrTerminator::Goto { target } => {
        let target_block = self.require_some(
          self.blocks.get(target).map(|r| *r.value()),
          "goto target block should have been pre-created",
        )?;

        self.require_llvm(
          self.builder.build_unconditional_branch(target_block),
          "unconditional branch",
        )?;
      }
      IrTerminator::Unreachable => {
        let _ = self.builder.build_unreachable();
      }
    }
    Ok(())
  }
}
