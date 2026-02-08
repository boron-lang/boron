use crate::parser::errors::{ExpectedPattern, InvalidFieldPattern};
use crate::parser::Parser;
use crate::{
  BoolLit, Expr, ExprKind, FieldPat, Literal, NodeId, Path, PathParsingContext, Pattern,
  PatternKind, RangeExpr, TokenType,
};
use boron_source::prelude::Span;

impl<'a> Parser<'a> {
  pub fn parse_pattern(&mut self) -> Pattern {
    self.parse_or_pattern()
  }

  fn parse_or_pattern(&mut self) -> Pattern {
    let start = self.current_span();
    let mut patterns = vec![self.parse_pattern_atom()];

    while self.eat(TokenType::Pipe) {
      patterns.push(self.parse_pattern_atom());
    }

    if patterns.len() == 1 {
      patterns.pop().expect("pattern should exist")
    } else {
      Pattern {
        id: NodeId::new(),
        span: self.span_from(start),
        kind: PatternKind::Or(patterns),
      }
    }
  }

  fn parse_pattern_atom(&mut self) -> Pattern {
    let start = self.current_span();
    let token = self.peek().clone();

    let kind = match &token.kind {
      TokenType::Underscore => {
        self.advance();
        PatternKind::Wildcard
      }
      TokenType::DotDot | TokenType::DotDotEq => self.parse_range_pattern(None, start),
      TokenType::True | TokenType::False => {
        let value = matches!(token.kind, TokenType::True);
        self.advance();
        let lit = Literal::Bool(BoolLit { id: NodeId::new(), value, span: token.span });
        if self.check(TokenType::DotDot) || self.check(TokenType::DotDotEq) {
          self.parse_range_pattern(
            Some(Expr::new(ExprKind::Literal(lit), token.span)),
            start,
          )
        } else {
          PatternKind::Literal(lit)
        }
      }

      TokenType::IntegerLiteral(..) => {
        let expr = self.parse_int_literal();
        if self.check(TokenType::DotDot) || self.check(TokenType::DotDotEq) {
          self.parse_range_pattern(Some(expr), start)
        } else if let ExprKind::Literal(lit) = expr.kind {
          PatternKind::Literal(lit)
        } else {
          PatternKind::Wildcard
        }
      }
      TokenType::StringLiteral(_) => {
        let expr = self.parse_string_literal();
        if self.check(TokenType::DotDot) || self.check(TokenType::DotDotEq) {
          self.parse_range_pattern(Some(expr), start)
        } else if let ExprKind::Literal(lit) = expr.kind {
          PatternKind::Literal(lit)
        } else {
          PatternKind::Wildcard
        }
      }
      TokenType::CharLiteral(_) => {
        let expr = self.parse_char_literal();
        if self.check(TokenType::DotDot) || self.check(TokenType::DotDotEq) {
          self.parse_range_pattern(Some(expr), start)
        } else if let ExprKind::Literal(lit) = expr.kind {
          PatternKind::Literal(lit)
        } else {
          PatternKind::Wildcard
        }
      }
      TokenType::LeftBracket => {
        self.advance();
        self.parse_slice_pattern()
      }
      TokenType::LeftParen => {
        self.advance();
        let mut patterns = vec![];

        while !self.check(TokenType::RightParen) && !self.is_at_end() {
          patterns.push(self.parse_pattern());
          if !self.eat(TokenType::Comma) {
            break;
          }
        }

        self.expect(TokenType::RightParen, "to close tuple pattern");

        PatternKind::Tuple(patterns)
      }
      TokenType::Mut => {
        self.advance();
        let name = self.parse_identifier();
        let subpat = self.parse_subpat();

        PatternKind::Binding { name, subpat, is_mut: true }
      }
      TokenType::Identifier(_) => {
        let path = self.parse_path(PathParsingContext::Normal);

        if self.eat(TokenType::LeftBrace) {
          self.parse_struct_pattern(path)
        } else if self.eat(TokenType::LeftParen) {
          self.parse_tuple_struct_pattern(path)
        } else if self.check(TokenType::DotDot) || self.check(TokenType::DotDotEq) {
          let expr = Expr::new(ExprKind::Path(path), self.span_from(start));
          self.parse_range_pattern(Some(expr), start)
        } else if path.segments.len() == 1 && path.root.is_none() {
          let name = path.segments[0].identifier;
          let subpat = self.parse_subpat();

          PatternKind::Binding { name, is_mut: false, subpat }
        } else {
          PatternKind::Path(path)
        }
      }
      _ => {
        self.emit(ExpectedPattern { found: token.kind.clone(), span: token.span });
        PatternKind::Wildcard
      }
    };

    Pattern { id: NodeId::new(), span: self.span_from(start), kind }
  }

  fn parse_subpat(&mut self) -> Option<Box<Pattern>> {
    if self.eat(TokenType::At) { Some(Box::new(self.parse_pattern())) } else { None }
  }

  fn parse_tuple_struct_pattern(&mut self, path: Path) -> PatternKind {
    let mut patterns = vec![];
    let kind = if self.eat(TokenType::DotDot) {
      PatternKind::TupleStruct { path, rest: true, patterns }
    } else {
      while !self.check(TokenType::RightParen) && !self.is_at_end() {
        patterns.push(self.parse_pattern());

        if !self.eat(TokenType::Comma) {
          break;
        }
      }

      PatternKind::TupleStruct { rest: false, patterns, path }
    };

    self.expect(TokenType::RightParen, "to close tuple struct pattern");
    kind
  }

  fn parse_struct_pattern(&mut self, path: Path) -> PatternKind {
    let mut fields = vec![];
    let kind = if self.eat(TokenType::DotDot) {
      PatternKind::Struct { path, fields, rest: true }
    } else {
      while !self.check(TokenType::RightBrace) && !self.is_at_end() {
        let field_start = self.current_span();

        let has_dot = self.eat(TokenType::Dot);
        if !has_dot && matches!(self.peek().kind, TokenType::Identifier(_)) {
          self.emit(InvalidFieldPattern { span: self.span_from(field_start) });
        }

        let name = match &self.peek().kind {
          TokenType::Identifier(_) => self.parse_identifier(),
          _ => break,
        };

        let separator = if has_dot { TokenType::Assign } else { TokenType::Colon };

        if self.eat(separator) {
          let pat = self.parse_pattern();
          fields.push(FieldPat {
            name,
            pat,
            id: NodeId::new(),
            span: self.span_from(field_start),
          });
        } else {
          fields.push(FieldPat {
            id: NodeId::new(),
            span: self.span_from(field_start),
            name,
            pat: Pattern {
              id: NodeId::new(),
              span: *name.span(),
              kind: PatternKind::Binding { name, subpat: None, is_mut: false },
            },
          });
        }

        if !self.eat(TokenType::Comma) {
          break;
        }
      }
      PatternKind::Struct { path, fields, rest: false }
    };
    self.expect(TokenType::RightBrace, "to close struct pattern");
    kind
  }

  fn parse_slice_pattern(&mut self) -> PatternKind {
    let mut prefix = vec![];
    let mut suffix = vec![];
    let mut middle: Option<Box<Pattern>> = None;
    let mut seen_rest = false;

    if self.eat(TokenType::RightBracket) {
      return PatternKind::Slice { prefix, middle, suffix };
    }

    while !self.check(TokenType::RightBracket) && !self.is_at_end() {
      if !seen_rest && self.check(TokenType::DotDot) {
        self.advance();
        seen_rest = true;

        if !self.check(TokenType::Comma) && !self.check(TokenType::RightBracket) {
          middle = Some(Box::new(self.parse_pattern()));
        }
      } else {
        let pat = self.parse_pattern();
        if seen_rest {
          suffix.push(pat);
        } else {
          prefix.push(pat);
        }
      }

      if !self.eat(TokenType::Comma) {
        break;
      }
    }

    self.expect(TokenType::RightBracket, "to close slice pattern");
    PatternKind::Slice { prefix, middle, suffix }
  }

  fn parse_range_pattern(
    &mut self,
    start_expr: Option<Expr>,
    start: Span,
  ) -> PatternKind {
    let inclusive = self.eat(TokenType::DotDotEq);
    if !inclusive {
      self.eat(TokenType::DotDot);
    }

    let end = if self.is_range_end_start() {
      Some(Box::new(self.parse_unary_expr()))
    } else {
      None
    };

    PatternKind::Range(RangeExpr {
      id: NodeId::new(),
      start: start_expr.map(Box::new),
      end,
      inclusive,
      span: self.span_from(start),
    })
  }
}
