use crate::parser::Parser;
use crate::parser::errors::{AllVarsInitialized, UseVarNotConst};
use crate::{Block, ExprStmt, NodeId, Statement, TokenType, VarDecl};

impl Parser<'_> {
  pub fn parse_block(&mut self) -> Block {
    let span = self.peek().span;
    self.expect(TokenType::LeftBrace, "to open block");
    let mut statements = vec![];

    if self.check(TokenType::RightBrace) {
      self.advance();
      return Block { id: NodeId::new(), span: self.span_from(span), statements };
    }

    loop {
      if let Some(stmt) = self.parse_statement() {
        statements.push(stmt);
      } else if !self.check(TokenType::Semicolon)
        && !self.check(TokenType::RightBrace)
        && !self.is_at_end()
      {
        self.advance();
      }

      if self.check(TokenType::Semicolon) {
        self.eat_semis();
        if self.check(TokenType::RightBrace) {
          break;
        }
      } else if self.check(TokenType::RightBrace) || self.is_at_end() {
        break;
      }
    }

    self.expect(TokenType::RightBrace, "to close block");

    Block { id: NodeId::new(), span: self.span_from(span), statements }
  }

  pub fn stmt_safe_boundary(&mut self) {
    self.advance_until_one_of(&[TokenType::Semicolon]);
    if self.check(TokenType::Semicolon) {
      self.eat_semis();
    }
  }

  pub fn parse_statement(&mut self) -> Option<Statement> {
    use TokenType::{Assign, Colon, Const, Semicolon, Var};
    let start_span = self.peek().span;

    match self.peek().kind {
      Const => {
        self.stmt_safe_boundary();

        let span = if self.peek().kind == Semicolon {
          self.span_from(start_span)
        } else {
          start_span
        };
        self.emit(UseVarNotConst { span });
        None
      }

      // in this context const just means the value isn't mutable
      Var => {
        self.advance();
        let pat = self.parse_pattern();
        let ty = if self.check(Colon) {
          self.advance();
          Some(self.parse_type())
        } else {
          None
        };

        if !self.check(Assign) {
          self.emit(AllVarsInitialized { span: self.span_from(start_span) });
          self.stmt_safe_boundary();
          return None;
        }
        self.eat(Assign);
        let value = self.parse_expr();

        Some(Statement::VarDecl(VarDecl {
          span: self.span_from(start_span),
          id: NodeId::new(),
          pat,
          value,
          ty,
        }))
      }
      _ => {
        let expr = self.parse_expr();

        let has_semicolon = self.check(Semicolon);
        if has_semicolon {
          self.eat_semis();
        }

        Some(Statement::Expr(ExprStmt {
          id: NodeId::new(),
          expr,
          span: self.span_from(start_span),
          has_semicolon,
        }))
      }
    }
  }
}
