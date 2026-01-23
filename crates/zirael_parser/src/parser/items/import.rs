use crate::import::ImportDecl;
use crate::parser::Parser;
use crate::parser::errors::{
  AliasingABinding, ImportNotAPath, UnexpectedImportKind,
};
use crate::{ImportKind, ImportSpec, NodeId, Path, TokenType};

impl Parser<'_> {
  pub(crate) fn parse_import(&mut self) -> Option<ImportDecl> {
    let span = self.previous().span;

    let kind = match self.peek().kind {
      TokenType::Star => ImportKind::Wildcard,
      TokenType::LeftBrace => {
        let mut specs = vec![];
        self.expect(TokenType::LeftBrace, "to open import list");

        loop {
          if self.check(TokenType::RightBrace) || self.is_at_end() {
            break;
          }

          let name = self.parse_identifier();

          let alias = if self.check(TokenType::As) {
            self.advance();
            let alias = self.parse_identifier();
            Some(alias)
          } else {
            None
          };

          specs.push(ImportSpec {
            id: NodeId::new(),
            name,
            alias,
            span: self.span_from(span),
          });

          if self.check(TokenType::Comma) {
            self.advance();
          } else {
            break;
          }
        }

        self.expect(TokenType::RightBrace, "to close import list");

        ImportKind::Items(specs)
      }
      TokenType::Identifier(_) => {
        let identifier = self.parse_identifier();

        if self.check(TokenType::As) {
          let span = self.peek().span;
          self.advance();
          let _ = self.parse_identifier();

          self.emit(AliasingABinding { span });
        }

        ImportKind::Binding(identifier)
      }
      _ => {
        self.emit(UnexpectedImportKind {
          span: self.peek().span,
          found: self.peek().kind.clone(),
        });
        self.synchronize_to_next_item();
        return None;
      }
    };
    self.expect(TokenType::From, "to indicate the import path");

    let path = if self.check_if(|t| matches!(t, &TokenType::StringLiteral(_))) {
      self.emit(ImportNotAPath {
        span: self.peek().span,
      });
      self.advance();
      Path::dummy()
    } else {
      self.parse_path()
    };

    self.eat_semis();

    Some(ImportDecl {
      id: NodeId::new(),
      span: self.span_from(span),
      path,
      kind,
    })
  }
}
