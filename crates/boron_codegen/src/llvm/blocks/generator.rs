use crate::llvm::LLVMCodegen;
use crate::llvm::blocks::BlockGeneratorContext;
use boron_ir::{IrStmt, IrStmtKind, IrTerminator};
use boron_utils::prelude::compiler_bug;

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

    if ctx.context == BlockContext::FunctionStart {
      for (idx, param) in ctx.ir_function.params.iter().enumerate() {
        let param_val =
          ctx.function.get_nth_param(idx as u32).expect("param index should exist");
        let alloca = self
          .builder
          .build_alloca(self.ty(&param.ty), &param.name)
          .expect("param alloca");

        if let Err(err) = self.builder.build_store(alloca, param_val) {
          compiler_bug!(self.ctx.dcx(), "failed to build param store instruction {}", err)
        }

        self.locals.insert(param.def_id, alloca);
      }
    }

    for stmt in &ctx.block.stmts {
      self.generate_stmt(stmt, ctx)
    }

    self.generate_terminator(&ctx.block.terminator);
  }

  pub fn generate_stmt(&self, stmt: &IrStmt, _ctx: &BlockGeneratorContext<'ctx, '_>) {
    match &stmt.kind {
      IrStmtKind::Local(local) => {
        let name = format!("local_{}", local.def_id.index());
        let p_val = self.builder.build_alloca(self.ty(&local.ty), &name).expect("alloca");

        self.locals.insert(local.def_id, p_val);

        let result = self
          .builder
          .build_store(p_val, self.generate_expr(&local.init.clone().unwrap()));

        match result {
          Ok(res) => {}
          Err(err) => {
            compiler_bug!(self.ctx.dcx(), "failed to build store instruction {}", err)
          }
        }
      }
      _ => unreachable!(),
    }
  }

  fn generate_terminator(&self, terminator: &IrTerminator) {
    match terminator {
      IrTerminator::Return(None) => {
        let _ = self.builder.build_return(None);
      }
      IrTerminator::Return(Some(val)) => {
        let _ = self.builder.build_return(Some(&self.generate_expr(val)));
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
  }
}
