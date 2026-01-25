use crate::parser::Parser;
use crate::parser::items::STRUCT_ITEM_TOKENS;
use crate::{NodeId, StructField, StructItem, StructMember, TokenType, Visibility};
use zirael_source::span::Span;

impl Parser<'_> {
  pub fn parse_struct(&mut self, span_start: Span) -> Option<StructItem> {
    let name = self.parse_identifier();
    let generics = self.parse_generic_parameters();
    let mut members = vec![];

    if self.check(TokenType::LeftBrace) {
      self.advance();

      loop {
        let span = self.peek().span;

        if self.check(TokenType::Pub)
          && let Some(ident) = self.peek_ahead(1)
          && let TokenType::Identifier(_) = &ident.kind
        {
          self.eat(TokenType::Pub);
          members.push(self.parse_field(span, Visibility::Public(span)));
          self.eat_commas();
        } else if let TokenType::Identifier(_) = &self.peek().kind {
          members.push(self.parse_field(span, Visibility::Private));
          self.eat_commas();
        } else if self.check(TokenType::RightBrace) {
          break;
        } else {
          self.doc_comment = None;
          self.check_possible_comment();

          let is_item_start = if self.check(TokenType::Pub) {
            self.check_next_any(STRUCT_ITEM_TOKENS)
          } else {
            self.check_any(STRUCT_ITEM_TOKENS)
          };

          let item = if is_item_start {
            self.parse_item()
          } else if self.check(TokenType::RightBrace) {
            break;
          } else {
            self.advance_until_one_of(
              &[STRUCT_ITEM_TOKENS, &[TokenType::RightBrace]].concat(),
            );
            None
          };

          if let Some(item) = item {
            members.push(StructMember::Item(item));
          } else {
            self.advance_until_one_of(
              &[STRUCT_ITEM_TOKENS, &[TokenType::RightBrace]].concat(),
            );
          }
        }
      }

      self.expect(TokenType::RightBrace, "to close struct declaration");
    }

    Some(StructItem {
      id: NodeId::new(),
      name,
      span: self.span_from(span_start),
      generics,
      members,
    })
  }

  pub fn parse_field(&mut self, span: Span, visibility: Visibility) -> StructMember {
    let name = self.parse_identifier();

    self.expect(TokenType::Colon, "to specify the field type");
    let ty = self.parse_type();

    StructMember::Field(StructField {
      id: NodeId::new(),
      span: self.span_from(span),
      name,
      ty,
      visibility,
      attributes: vec![],
    })
  }
}
