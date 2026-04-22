use crate::parser::Parser;
use crate::parser::errors::InvalidVariantStart;
use crate::parser::items::ADT_ITEM_TOKENS;
use boron_source::span::Span;
use boron_types::ast::{
  EnumItem, EnumMember, EnumVariantStructField, NodeId, Variant, VariantPayload,
};
use boron_types::tokens::TokenType;

impl Parser<'_> {
  pub fn parse_enum(&mut self, span_start: Span) -> Option<EnumItem> {
    let name = self.parse_identifier();
    let generics = self.parse_generic_parameters();
    self.expect(TokenType::LeftBrace, "to open enum body");
    let members = self.parse_enum_members();
    self.expect(TokenType::RightBrace, "to close enum body");

    Some(EnumItem {
      id: NodeId::new(),
      span: self.span_from(span_start),
      name,
      generics,
      members,
    })
  }

  fn parse_enum_members(&mut self) -> Vec<EnumMember> {
    let span = self.current_span();
    let mut members = vec![];

    loop {
      if self.check(TokenType::RightBrace) || self.is_at_end() {
        break;
      }

      self.doc_comment = None;
      self.check_possible_comment();

      let is_item_start = if self.check(TokenType::Pub) {
        self.check_next_any(ADT_ITEM_TOKENS)
      } else {
        self.check_any(ADT_ITEM_TOKENS)
      };

      if is_item_start {
        let item = self.parse_item();

        if let Some(item) = item {
          members.push(EnumMember::Item(item));
        } else {
          while !self.is_at_end() {
            let is_item_start = if self.check(TokenType::Pub) {
              self.check_next_any(ADT_ITEM_TOKENS)
            } else {
              self.check_any(ADT_ITEM_TOKENS)
            };

            if self.check(TokenType::RightBrace) || self.is_identifier() || is_item_start
            {
              break;
            }

            self.advance();
          }

          self.emit(InvalidVariantStart {
            span: self.peek().span,
            found: self.peek().kind.clone(),
          });
        }

        self.advance_until_one_of(&[TokenType::Comma, TokenType::RightBrace]);
        self.eat(TokenType::Comma);
        continue;
      }

      if !self.is_identifier() {
        while !self.is_at_end() {
          let is_item_start = if self.check(TokenType::Pub) {
            self.check_next_any(ADT_ITEM_TOKENS)
          } else {
            self.check_any(ADT_ITEM_TOKENS)
          };

          if self.check(TokenType::RightBrace) || self.is_identifier() || is_item_start {
            break;
          }

          self.advance();
        }
        continue;
      }

      // TODO: attributes
      // let attributes = self.parse_attributes()?;
      let name = self.parse_identifier();

      let payload = match self.peek().kind {
        TokenType::Comma => {
          self.eat(TokenType::Comma);
          EnumMember::Variant(Variant {
            payload: VariantPayload::Unit,
            id: NodeId::new(),
            name,
            attributes: vec![],
            span: self.span_from(span),
          })
        }
        TokenType::LeftParen => {
          self.eat(TokenType::LeftParen);
          let mut types = vec![];

          loop {
            if self.check(TokenType::RightParen) || self.is_at_end() {
              break;
            }

            types.push(self.parse_type());
            self.eat(TokenType::Comma);
          }
          self.expect(TokenType::RightParen, "to close tuple variant");
          self.eat(TokenType::Comma);

          EnumMember::Variant(Variant {
            payload: VariantPayload::Tuple(types),
            id: NodeId::new(),
            name,
            attributes: vec![],
            span: self.span_from(span),
          })
        }
        TokenType::LeftBrace => {
          self.eat(TokenType::LeftBrace);
          let mut fields = vec![];

          loop {
            if self.check(TokenType::RightBrace) || self.is_at_end() {
              break;
            }
            let field_span = self.current_span();
            self.expect(TokenType::Dot, "dot has to start a struct field");
            let identifier = self.parse_identifier();
            self.expect(TokenType::Assign, "");
            let ty = self.parse_type();
            fields.push(EnumVariantStructField {
              name: identifier,
              span: field_span.to(self.current_span()),
              id: NodeId::new(),
              ty,
            });

            self.eat(TokenType::Comma);
          }

          self.expect(TokenType::RightBrace, "to close struct variant");
          self.eat(TokenType::Comma);

          EnumMember::Variant(Variant {
            payload: VariantPayload::Struct(fields),
            id: NodeId::new(),
            name,
            attributes: vec![],
            span: self.span_from(span),
          })
        }
        _ => {
          while !self.is_at_end() {
            let is_item_start = if self.check(TokenType::Pub) {
              self.check_next_any(ADT_ITEM_TOKENS)
            } else {
              self.check_any(ADT_ITEM_TOKENS)
            };

            if self.check(TokenType::RightBrace) || self.is_identifier() || is_item_start
            {
              break;
            }

            self.advance();
          }
          continue;
        }
      };

      members.push(payload);
    }

    members
  }
}
