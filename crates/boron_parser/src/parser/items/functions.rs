use crate::parser::Parser;
use crate::parser::errors::{ExpectedParenToOpenList, ExternNoBody, FunctionCamelCase};
use crate::{
  FunctionItem, FunctionModifiers, NeverType, NodeId, TokenType, Type, UnitType,
};
use boron_source::ident_table::Identifier;
use boron_source::prelude::Span;
use stringcase::camel_case;

impl Parser<'_> {
  pub(crate) fn parse_function(
    &mut self,
    modifiers: FunctionModifiers,
    span_start: Span,
  ) -> Option<FunctionItem> {
    let name = self.parse_identifier();
    self.validate_function_name(name);
    let generics = self.parse_generic_parameters();

    let has_parens = self.eat(TokenType::LeftParen);
    if !has_parens {
      self.emit(ExpectedParenToOpenList { span: self.peek().span });
      self.advance_until_one_of(&[
        TokenType::Arrow,
        TokenType::LeftBrace,
        TokenType::Semicolon,
      ]);
    }

    let params = if has_parens {
      let params = self.parse_function_parameters();
      self.expect(TokenType::RightParen, "to close parameter list");
      params
    } else {
      vec![]
    };

    let return_type = if self.check(TokenType::Arrow) {
      self.advance();
      if self.eat(TokenType::Not) {
        // TODO: diagnostic on invalid usage in normal types
        // `!` type is only allowed in the function return type
        Type::Never(NeverType { id: NodeId::new(), span: self.previous().span })
      } else {
        self.parse_type()
      }
    } else {
      Type::Unit(UnitType { id: NodeId::new(), span: Span::dummy() })
    };

    let body =
      if self.check(TokenType::LeftBrace) { Some(self.parse_block()) } else { None };

    if let Some(body) = &body
      && modifiers.external.is_some()
    {
      self.emit(ExternNoBody { span: body.span });
    }

    Some(FunctionItem {
      id: NodeId::new(),
      modifiers,
      name,
      generics,
      params,
      return_type,
      body,
      span: self.span_from(span_start),
    })
  }

  fn validate_function_name(&self, name: Identifier) {
    if name.text() != camel_case(&name.text()) {
      self.emit(FunctionCamelCase { span: *name.span() });
    }
  }
}
