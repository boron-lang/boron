use crate::{
  IrBlock, IrBody, IrExpr, IrExprKind, IrFieldInit, IrStmt, IrStmtKind, IrTerminator,
};
use crate::{IrLocal, IrLowerer};
use boron_analysis::ty::SubstitutionMap;
use boron_hir::{HirId, LocalId};
use boron_resolver::DefId;
use boron_thir::{
  Block as ThirBlock, Expr as ThirExpr, ExprKind as ThirExprKind,
  FieldInit as ThirFieldInit, StmtKind as ThirStmtKind,
};

#[derive(Clone, Copy, Debug)]
struct LoopContext {
  break_target: HirId,
  continue_target: HirId,
}

#[derive(Debug)]
pub(crate) struct CfgLoweringContext {
  owner: DefId,
  type_args: SubstitutionMap,
  loop_stack: Vec<LoopContext>,
}

impl CfgLoweringContext {
  fn new(owner: DefId, type_args: SubstitutionMap) -> Self {
    Self { owner, type_args, loop_stack: Vec::new() }
  }
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

impl IrLowerer<'_> {
  pub(crate) fn lower_body(
    &mut self,
    block: &ThirBlock,
    type_args: &SubstitutionMap,
    owner: DefId,
  ) -> IrBody {
    self.set_cfg_context(CfgLoweringContext::new(owner, type_args.clone()));

    let owner = self.cfg_context().owner;
    let mut builder = CfgBuilder::new(owner, block.hir_id.local_id.next());

    self.lower_block_with_entry(&mut builder, block.hir_id, block, None);

    self.clear_cfg_context();

    IrBody { entry: block.hir_id, blocks: builder.blocks }
  }

  fn lower_block_with_entry(
    &mut self,
    builder: &mut CfgBuilder,
    entry_id: HirId,
    block: &ThirBlock,
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
          let type_args = &self.cfg_context().type_args;
          let ty = Self::lower_semantic_ty(&local.ty, type_args);
          let mut projections = vec![];
          let hir_id = local.hir_id;

          self.lower_local_pattern(&ty, &local.pat, &mut projections);

          let init = local.init.clone().unwrap();
          let local = IrLocal {
            hir_id: local.hir_id,
            ty,
            init: self.lower_cfg_expr_or_skip(
              builder,
              &mut current_id,
              &mut current_stmts,
              &mut terminator,
              &init,
            ),
            span: local.span,
            projections,
          };
          self.ir.add_local(self.current_function, local);

          current_stmts.push(IrStmt {
            hir_id: stmt.hir_id,
            kind: IrStmtKind::Local(hir_id),
            span: stmt.span,
          });
        }
        ThirStmtKind::Expr(expr) => {
          if let Some(lowered) = self.handle_control_expr(
            builder,
            &mut current_id,
            &mut current_stmts,
            &mut terminator,
            expr,
          ) {
            current_stmts.push(IrStmt {
              hir_id: stmt.hir_id,
              kind: IrStmtKind::Expr(lowered),
              span: stmt.span,
            });
          }
        }
      }
    }

    if terminator.is_none() {
      if let Some(e) = block.expr.as_ref() {
        if let Some(val) = self.handle_control_expr(
          builder,
          &mut current_id,
          &mut current_stmts,
          &mut terminator,
          e,
        ) {
          terminator = Some(IrTerminator::Return(Some(val)));
        }
      } else {
        terminator = Some(IrTerminator::Return(None));
      }
    }

    let terminator = match fallthrough {
      Some(target) => match terminator {
        Some(
          IrTerminator::Branch { .. }
          | IrTerminator::Goto { .. }
          | IrTerminator::Return(_)
          | IrTerminator::Unreachable,
        ) => terminator,
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
    &mut self,
    builder: &mut CfgBuilder,
    current_id: &mut HirId,
    current_stmts: &mut Vec<IrStmt>,
    terminator: &mut Option<IrTerminator>,
    expr: &ThirExpr,
  ) -> Option<IrExpr> {
    match &expr.kind {
      ThirExprKind::Return { value } => {
        let value = value.as_ref().map(|e| {
          self.lower_cfg_expr_or_skip(builder, current_id, current_stmts, terminator, e)
        });
        *terminator = Some(IrTerminator::Return(value));
        Self::flush_block(
          builder,
          *current_id,
          std::mem::take(current_stmts),
          terminator.clone(),
          expr,
        );
        None
      }
      ThirExprKind::Break { value } => {
        if value.is_some() {
          todo!("lower break with value into CFG");
        }
        let target = self
          .cfg_context()
          .loop_stack
          .last()
          .map(|ctx| ctx.break_target)
          .unwrap_or_else(|| todo!("break outside of loop"));
        *terminator = Some(IrTerminator::Goto { target });
        Self::flush_block(
          builder,
          *current_id,
          std::mem::take(current_stmts),
          terminator.clone(),
          expr,
        );
        None
      }
      ThirExprKind::Continue => {
        let target = self
          .cfg_context()
          .loop_stack
          .last()
          .map(|ctx| ctx.continue_target)
          .unwrap_or_else(|| todo!("continue outside of loop"));
        *terminator = Some(IrTerminator::Goto { target });
        Self::flush_block(
          builder,
          *current_id,
          std::mem::take(current_stmts),
          terminator.clone(),
          expr,
        );
        None
      }
      ThirExprKind::If { condition, then_block, else_branch } => {
        let then_id = builder.new_block_id();
        let else_id = builder.new_block_id();
        let join_id = builder.new_block_id();

        let cond = self.lower_cfg_expr_or_skip(
          builder,
          current_id,
          current_stmts,
          terminator,
          condition,
        );
        let branch = IrTerminator::Branch {
          condition: cond,
          then_target: then_id,
          else_target: else_id,
        };

        Self::flush_block(
          builder,
          *current_id,
          std::mem::take(current_stmts),
          Some(branch),
          expr,
        );

        self.lower_block_with_entry(builder, then_id, then_block, Some(join_id));

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
            self.lower_block_with_entry(builder, else_id, block, Some(join_id));
          }
          Some(_) => {
            let else_expr = else_branch.as_ref().unwrap();
            let stmt = IrStmt {
              hir_id: else_expr.hir_id,
              kind: IrStmtKind::Expr(self.lower_cfg_expr_or_skip(
                builder,
                current_id,
                current_stmts,
                terminator,
                else_expr,
              )),
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
        None
      }
      ThirExprKind::Block(block) => {
        let block_id = builder.new_block_id();
        let join_id = builder.new_block_id();

        Self::flush_block(
          builder,
          *current_id,
          std::mem::take(current_stmts),
          Some(IrTerminator::Goto { target: block_id }),
          expr,
        );

        self.lower_block_with_entry(builder, block_id, block, Some(join_id));

        *current_id = join_id;
        *terminator = None;
        None
      }
      ThirExprKind::Loop { body } => {
        let loop_body_id = builder.new_block_id();
        let loop_exit_id = builder.new_block_id();

        Self::flush_block(
          builder,
          *current_id,
          std::mem::take(current_stmts),
          Some(IrTerminator::Goto { target: loop_body_id }),
          expr,
        );

        {
          let ctx = self.cfg_context_mut();
          ctx.loop_stack.push(LoopContext {
            break_target: loop_exit_id,
            continue_target: loop_body_id,
          });
        }

        self.lower_block_with_entry(builder, loop_body_id, body, Some(loop_body_id));

        {
          let ctx = self.cfg_context_mut();
          ctx.loop_stack.pop();
        }

        *current_id = loop_exit_id;
        *terminator = None;
        None
      }
      ThirExprKind::Match { .. } => {
        todo!("lower match into CFG");
      }
      ThirExprKind::Literal(lit) => Some(IrExpr {
        hir_id: expr.hir_id,
        ty: Self::lower_semantic_ty(&expr.ty, &self.cfg_context().type_args),
        kind: IrExprKind::Literal(lit.clone()),
        span: expr.span,
      }),
      ThirExprKind::LocalRef(def_id) => Some(IrExpr {
        hir_id: expr.hir_id,
        ty: Self::lower_semantic_ty(&expr.ty, &self.cfg_context().type_args),
        kind: IrExprKind::LocalRef(*def_id),
        span: expr.span,
      }),
      ThirExprKind::Path(def_id) => Some(IrExpr {
        hir_id: expr.hir_id,
        ty: Self::lower_semantic_ty(&expr.ty, &self.cfg_context().type_args),
        kind: IrExprKind::GlobalRef(*def_id),
        span: expr.span,
      }),
      ThirExprKind::Binary { op, lhs, rhs } => {
        let lhs = self.lower_cfg_expr_or_skip(
          builder,
          current_id,
          current_stmts,
          terminator,
          lhs,
        );
        let rhs = self.lower_cfg_expr_or_skip(
          builder,
          current_id,
          current_stmts,
          terminator,
          rhs,
        );
        Some(IrExpr {
          hir_id: expr.hir_id,
          ty: Self::lower_semantic_ty(&expr.ty, &self.cfg_context().type_args),
          kind: IrExprKind::Binary { op: *op, lhs: Box::new(lhs), rhs: Box::new(rhs) },
          span: expr.span,
        })
      }
      ThirExprKind::Unary { op, operand } => {
        let operand = self.lower_cfg_expr_or_skip(
          builder,
          current_id,
          current_stmts,
          terminator,
          operand,
        );
        Some(IrExpr {
          hir_id: expr.hir_id,
          ty: Self::lower_semantic_ty(&expr.ty, &self.cfg_context().type_args),
          kind: IrExprKind::Unary { op: *op, operand: Box::new(operand) },
          span: expr.span,
        })
      }
      ThirExprKind::Assign { target, value } => {
        let target = self.lower_cfg_expr_or_skip(
          builder,
          current_id,
          current_stmts,
          terminator,
          target,
        );
        let value = self.lower_cfg_expr_or_skip(
          builder,
          current_id,
          current_stmts,
          terminator,
          value,
        );
        Some(IrExpr {
          hir_id: expr.hir_id,
          ty: Self::lower_semantic_ty(&expr.ty, &self.cfg_context().type_args),
          kind: IrExprKind::Assign { target: Box::new(target), value: Box::new(value) },
          span: expr.span,
        })
      }
      ThirExprKind::Cast { expr: inner, ty } => {
        let inner = self.lower_cfg_expr_or_skip(
          builder,
          current_id,
          current_stmts,
          terminator,
          inner,
        );
        let cast_ty = Self::lower_semantic_ty(ty, &self.cfg_context().type_args);
        Some(IrExpr {
          hir_id: expr.hir_id,
          ty: Self::lower_semantic_ty(&expr.ty, &self.cfg_context().type_args),
          kind: IrExprKind::Cast { expr: Box::new(inner), ty: cast_ty },
          span: expr.span,
        })
      }
      ThirExprKind::Call { callee, type_args: call_type_args, args } => {
        let args = args
          .iter()
          .map(|arg| {
            self.lower_cfg_expr_or_skip(
              builder,
              current_id,
              current_stmts,
              terminator,
              arg,
            )
          })
          .collect();
        let type_args = call_type_args
          .iter()
          .map(|ty| Self::lower_semantic_ty(ty, &self.cfg_context().type_args))
          .collect();
        Some(IrExpr {
          hir_id: expr.hir_id,
          ty: Self::lower_semantic_ty(&expr.ty, &self.cfg_context().type_args),
          kind: IrExprKind::Call { callee: *callee, type_args, args },
          span: expr.span,
        })
      }
      ThirExprKind::Field { object, field } => {
        let object = self.lower_cfg_expr_or_skip(
          builder,
          current_id,
          current_stmts,
          terminator,
          object,
        );
        Some(IrExpr {
          hir_id: expr.hir_id,
          ty: Self::lower_semantic_ty(&expr.ty, &self.cfg_context().type_args),
          kind: IrExprKind::Field { object: Box::new(object), field: *field },
          span: expr.span,
        })
      }
      ThirExprKind::Index { object, index } => {
        let object = self.lower_cfg_expr_or_skip(
          builder,
          current_id,
          current_stmts,
          terminator,
          object,
        );
        let index = self.lower_cfg_expr_or_skip(
          builder,
          current_id,
          current_stmts,
          terminator,
          index,
        );
        Some(IrExpr {
          hir_id: expr.hir_id,
          ty: Self::lower_semantic_ty(&expr.ty, &self.cfg_context().type_args),
          kind: IrExprKind::Index { object: Box::new(object), index: Box::new(index) },
          span: expr.span,
        })
      }
      ThirExprKind::AddrOf { operand } => {
        let operand = self.lower_cfg_expr_or_skip(
          builder,
          current_id,
          current_stmts,
          terminator,
          operand,
        );
        Some(IrExpr {
          hir_id: expr.hir_id,
          ty: Self::lower_semantic_ty(&expr.ty, &self.cfg_context().type_args),
          kind: IrExprKind::AddrOf { operand: Box::new(operand) },
          span: expr.span,
        })
      }
      ThirExprKind::Struct { def_id, type_args: struct_type_args, fields } => {
        let fields = fields
          .iter()
          .map(|field| {
            self.lower_cfg_field_init(
              builder,
              current_id,
              current_stmts,
              terminator,
              field,
            )
          })
          .collect();
        let type_args = struct_type_args
          .iter()
          .map(|ty| Self::lower_semantic_ty(ty, &self.cfg_context().type_args))
          .collect();
        Some(IrExpr {
          hir_id: expr.hir_id,
          ty: Self::lower_semantic_ty(&expr.ty, &self.cfg_context().type_args),
          kind: IrExprKind::Struct { def_id: *def_id, type_args, fields },
          span: expr.span,
        })
      }
      ThirExprKind::Tuple(exprs) => {
        let exprs = exprs
          .iter()
          .map(|e| {
            self.lower_cfg_expr_or_skip(builder, current_id, current_stmts, terminator, e)
          })
          .collect();
        Some(IrExpr {
          hir_id: expr.hir_id,
          ty: Self::lower_semantic_ty(&expr.ty, &self.cfg_context().type_args),
          kind: IrExprKind::Tuple(exprs),
          span: expr.span,
        })
      }
      ThirExprKind::Array(exprs) => {
        let exprs = exprs
          .iter()
          .map(|e| {
            self.lower_cfg_expr_or_skip(builder, current_id, current_stmts, terminator, e)
          })
          .collect();
        Some(IrExpr {
          hir_id: expr.hir_id,
          ty: Self::lower_semantic_ty(&expr.ty, &self.cfg_context().type_args),
          kind: IrExprKind::Array(exprs),
          span: expr.span,
        })
      }
      ThirExprKind::Err => panic!("err expr shouldn't exist at this point: {:#?}", expr),
    }
  }

  fn lower_cfg_expr_or_skip(
    &mut self,
    builder: &mut CfgBuilder,
    current_id: &mut HirId,
    current_stmts: &mut Vec<IrStmt>,
    terminator: &mut Option<IrTerminator>,
    expr: &ThirExpr,
  ) -> IrExpr {
    self
      .handle_control_expr(builder, current_id, current_stmts, terminator, expr)
      .unwrap_or_else(|| IrExpr {
        hir_id: expr.hir_id,
        ty: Self::lower_semantic_ty(&expr.ty, &self.cfg_context().type_args),
        kind: IrExprKind::Skip,
        span: expr.span,
      })
  }

  fn lower_cfg_field_init(
    &mut self,
    builder: &mut CfgBuilder,
    current_id: &mut HirId,
    current_stmts: &mut Vec<IrStmt>,
    terminator: &mut Option<IrTerminator>,
    field: &ThirFieldInit,
  ) -> IrFieldInit {
    let type_args = &self.cfg_context().type_args;
    let ty = Self::lower_semantic_ty(&field.ty, type_args);

    IrFieldInit {
      hir_id: field.hir_id,
      name: field.name.text(),
      ty,
      value: self.lower_cfg_expr_or_skip(
        builder,
        current_id,
        current_stmts,
        terminator,
        &field.value,
      ),
      span: field.span,
    }
  }

  fn flush_block(
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
