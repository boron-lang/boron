// TODO:
// - `for` loops -> `loop` + iterator protocol
// - `while` loops -> `loop` + `if` + `break`
// - `?` operator -> `match` with early return
// - Method calls -> function calls with self
// - Operator overloading -> trait method calls

mod context;
mod expr;
mod item;
mod pat;
mod ty;

pub use context::LoweringContext;

use boron_context::BCtx;
use boron_diagnostics::DiagnosticCtx;

pub fn lower_to_hir<'ctx>(ctx: &'ctx BCtx<'ctx>, _dcx: &DiagnosticCtx) {
  let hir = ctx.hir();

  for module in ctx.modules().all() {
    let mut ctx = LoweringContext::new(ctx.resolver(), hir, module.source_file_id);
    ctx.lower_module(&module.node);
  }
}
