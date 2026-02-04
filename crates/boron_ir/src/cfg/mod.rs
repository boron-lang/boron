mod lowerer;

use crate::IrExpr;
use boron_hir::HirId;

#[derive(Debug, Clone)]
pub enum IrTerminator {
  Return(Option<IrExpr>),
  Branch { condition: IrExpr, then_target: HirId, else_target: HirId },
  Goto { target: HirId },
  Unreachable,
}
