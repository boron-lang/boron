use crate::codegen::LLVMCodegen;
use anyhow::Result;
use boron_ir::{IrBlock, IrStmt, IrStmtKind};

impl LLVMCodegen<'_> {
  pub fn generate_block(&self, block: &IrBlock) -> Result<()> {
    for stmt in &block.stmts {
      self.generate_stmt(stmt)?;
    }
    Ok(())
  }

  pub fn generate_stmt(&self, stmt: &IrStmt) -> Result<()> {
    match &stmt.kind {
      IrStmtKind::Expr(expr) => {
        self.generate_expr(expr)?;
      }
      IrStmtKind::Local(_) => {}
    }
    Ok(())
  }
}
