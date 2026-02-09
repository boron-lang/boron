use crate::{IrBlock, IrBody, IrStmt, IrStmtKind, IrTerminator};
use crate::{IrLocal, IrLowerer};
use boron_analysis::ty::SubstitutionMap;
use boron_hir::{HirId, LocalId};
use boron_resolver::DefId;
use boron_thir::{
  Block as ThirBlock, Expr as ThirExpr, ExprKind as ThirExprKind,
  StmtKind as ThirStmtKind,
};

#[derive(Clone, Copy)]
struct LoopContext {
  break_target: HirId,
  continue_target: HirId,
}

struct CfgBuilder {
  owner: DefId,
  next_local: LocalId,
  blocks: Vec<IrBlock>,
}

impl CfgBuilder {
  fn new(owner: DefId, start_local: LocalId) -> Self {
    Self { owner, next_local: start_local, blocks: Vec::new() }
  }

  fn new_block_id(&mut self) -> HirId {
    let id = HirId::new(self.owner, self.next_local);
    self.next_local = self.next_local.next();
    id
  }
}

impl<'a> IrLowerer<'a> {
  pub(crate) fn lower_body(
    &self,
    block: &ThirBlock,
    type_args: &SubstitutionMap,
    owner: DefId,
  ) -> IrBody {
    let mut builder = CfgBuilder::new(owner, block.hir_id.local_id.next());
    let mut loop_stack = Vec::new();

    self.lower_block_with_entry(
      &mut builder,
      block.hir_id,
      block,
      type_args,
      &mut loop_stack,
      None,
    );

    IrBody { entry: block.hir_id, blocks: builder.blocks }
  }

  fn lower_block_with_entry(
    &self,
    builder: &mut CfgBuilder,
    entry_id: HirId,
    block: &ThirBlock,
    type_args: &SubstitutionMap,
    loop_stack: &mut Vec<LoopContext>,
    fallthrough: Option<HirId>,
  ) {
    let mut current_id = entry_id;
    let mut current_stmts = Vec::new();
    let mut terminator: Option<IrTerminator> = None;

    for stmt in &block.stmts {
      if terminator.is_some() {
        break;
      }

      match &stmt.kind {
        ThirStmtKind::Local(local) => {
          let ty = self.lower_semantic_ty(&local.ty, type_args);
          let mut projections = vec![];
          let hir_id = local.hir_id;

          self.lower_local_pattern(&ty, &local.pat, &mut projections);

          let init = local.init.clone().unwrap();
          let local = IrLocal {
            hir_id: local.hir_id,
            ty,
            init: self.lower_expr(&init, type_args),
            span: local.span,
            projections,
          };
          self.ir.add_local(self.current_function, local);

          current_stmts.push(IrStmt {
            hir_id: stmt.hir_id,
            kind: IrStmtKind::Local(hir_id),
            span: stmt.span,
          })
        }
        ThirStmtKind::Expr(expr) => {
          if self.handle_control_expr(
            builder,
            &mut current_id,
            &mut current_stmts,
            &mut terminator,
            expr,
            type_args,
            loop_stack,
          ) {
            continue;
          }

          current_stmts.push(IrStmt {
            hir_id: stmt.hir_id,
            kind: IrStmtKind::Expr(self.lower_expr(expr, type_args)),
            span: stmt.span,
          });
        }
      }
    }

    if terminator.is_none() {
      let ret = block.expr.as_ref().map(|e| self.lower_expr(e, type_args));
      terminator = Some(IrTerminator::Return(ret));
    }

    let terminator = match fallthrough {
      Some(target) => match terminator {
        Some(IrTerminator::Return(_)) | Some(IrTerminator::Unreachable) => terminator,
        Some(IrTerminator::Branch { .. }) | Some(IrTerminator::Goto { .. }) => terminator,
        None => Some(IrTerminator::Goto { target }),
      },
      None => terminator,
    };

    builder.blocks.push(IrBlock {
      hir_id: current_id,
      stmts: current_stmts,
      terminator: terminator.unwrap_or(IrTerminator::Unreachable),
      span: block.span,
    });
  }

  fn handle_control_expr(
    &self,
    builder: &mut CfgBuilder,
    current_id: &mut HirId,
    current_stmts: &mut Vec<IrStmt>,
    terminator: &mut Option<IrTerminator>,
    expr: &ThirExpr,
    type_args: &SubstitutionMap,
    loop_stack: &mut Vec<LoopContext>,
  ) -> bool {
    match &expr.kind {
      ThirExprKind::Return { value } => {
        let value = value.as_ref().map(|e| self.lower_expr(e, type_args));
        *terminator = Some(IrTerminator::Return(value));
        self.flush_block(
          builder,
          *current_id,
          std::mem::take(current_stmts),
          terminator.clone(),
          expr,
        );
        true
      }
      ThirExprKind::Break { value } => {
        if value.is_some() {
          todo!("lower break with value into CFG");
        }
        let target = loop_stack
          .last()
          .map(|ctx| ctx.break_target)
          .unwrap_or_else(|| todo!("break outside of loop"));
        *terminator = Some(IrTerminator::Goto { target });
        self.flush_block(
          builder,
          *current_id,
          std::mem::take(current_stmts),
          terminator.clone(),
          expr,
        );
        true
      }
      ThirExprKind::Continue => {
        let target = loop_stack
          .last()
          .map(|ctx| ctx.continue_target)
          .unwrap_or_else(|| todo!("continue outside of loop"));
        *terminator = Some(IrTerminator::Goto { target });
        self.flush_block(
          builder,
          *current_id,
          std::mem::take(current_stmts),
          terminator.clone(),
          expr,
        );
        true
      }
      ThirExprKind::If { condition, then_block, else_branch } => {
        let then_id = builder.new_block_id();
        let else_id = builder.new_block_id();
        let join_id = builder.new_block_id();

        let cond = self.lower_expr(condition, type_args);
        let branch = IrTerminator::Branch {
          condition: cond,
          then_target: then_id,
          else_target: else_id,
        };

        self.flush_block(
          builder,
          *current_id,
          std::mem::take(current_stmts),
          Some(branch),
          expr,
        );

        self.lower_block_with_entry(
          builder,
          then_id,
          then_block,
          type_args,
          loop_stack,
          Some(join_id),
        );

        match else_branch.as_ref().map(|e| &e.kind) {
          None => {
            builder.blocks.push(IrBlock {
              hir_id: else_id,
              stmts: Vec::new(),
              terminator: IrTerminator::Goto { target: join_id },
              span: expr.span,
            });
          }
          Some(ThirExprKind::Block(block)) => {
            self.lower_block_with_entry(
              builder,
              else_id,
              block,
              type_args,
              loop_stack,
              Some(join_id),
            );
          }
          Some(_) => {
            let else_expr = else_branch.as_ref().unwrap();
            let stmt = IrStmt {
              hir_id: else_expr.hir_id,
              kind: IrStmtKind::Expr(self.lower_expr(else_expr, type_args)),
              span: else_expr.span,
            };
            builder.blocks.push(IrBlock {
              hir_id: else_id,
              stmts: vec![stmt],
              terminator: IrTerminator::Goto { target: join_id },
              span: expr.span,
            });
          }
        }

        *current_id = join_id;
        *terminator = None;
        true
      }
      ThirExprKind::Loop { body } => {
        let loop_body_id = builder.new_block_id();
        let loop_exit_id = builder.new_block_id();

        self.flush_block(
          builder,
          *current_id,
          std::mem::take(current_stmts),
          Some(IrTerminator::Goto { target: loop_body_id }),
          expr,
        );

        loop_stack.push(LoopContext {
          break_target: loop_exit_id,
          continue_target: loop_body_id,
        });

        self.lower_block_with_entry(
          builder,
          loop_body_id,
          body,
          type_args,
          loop_stack,
          Some(loop_body_id),
        );

        loop_stack.pop();

        *current_id = loop_exit_id;
        *terminator = None;
        true
      }
      ThirExprKind::Match { .. } => {
        todo!("lower match into CFG");
      }
      _ => false,
    }
  }

  fn flush_block(
    &self,
    builder: &mut CfgBuilder,
    block_id: HirId,
    stmts: Vec<IrStmt>,
    terminator: Option<IrTerminator>,
    expr: &ThirExpr,
  ) {
    if let Some(terminator) = terminator {
      builder.blocks.push(IrBlock {
        hir_id: block_id,
        stmts,
        terminator,
        span: expr.span,
      });
    }
  }
}
