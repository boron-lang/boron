use crate::parser::Parser;
use crate::parser::errors::{
  ExpectedIdentifierInNormalPath, ExpectedSuperOrIdentPath, GenericsInImportOrMod,
  PackageInRootOnly, SuperOnlyInModOrImport,
};
use crate::{NodeId, Path, PathParsingContext, PathRoot, PathSegment, TokenType};

impl Parser<'_> {
  pub fn parse_path(&mut self, ctx: PathParsingContext) -> Path {
    let start = self.current_span();
    let mut segments = Vec::new();

    let root = if self.eat(TokenType::Package) {
      Some(PathRoot::Package)
    } else if self.eat(TokenType::SelfValue) {
      Some(PathRoot::SelfMod)
    } else if self.eat(TokenType::Super) {
      Some(PathRoot::Super)
    } else if self.is_identifier() {
      let identifier = self.parse_identifier();
      let (span, generics) = self.parse_generic_arguments();

      if ctx == PathParsingContext::ImportOrMod && !generics.is_empty() {
        self.emit(GenericsInImportOrMod { span });
      }
      segments.push(PathSegment { identifier, args: generics });

      None
    } else {
      None
    };

    while self.eat(TokenType::ColonColon) {
      if self.check(TokenType::Lt) || self.check(TokenType::LeftBrace) {
        break;
      }

      match self.peek().kind {
        TokenType::Super => {
          let identifier = self.parse_identifier();
          let (span, generics) = self.parse_generic_arguments();

          if ctx == PathParsingContext::Normal {
            self.emit(SuperOnlyInModOrImport { span: *identifier.span() });
          }

          if !generics.is_empty() {
            self.emit(GenericsInImportOrMod { span });
          }

          segments.push(PathSegment { identifier, args: vec![] });
          self.advance();
        }
        TokenType::Package => {
          self.emit(PackageInRootOnly { span: self.peek().span });
          self.advance();
        }
        TokenType::Identifier(_) => {
          let identifier = self.parse_identifier();
          let (span, generics) = self.parse_generic_arguments();

          if !generics.is_empty() && ctx == PathParsingContext::ImportOrMod {
            self.emit(GenericsInImportOrMod { span });
          }

          segments.push(PathSegment { identifier, args: generics });
        }
        _ => {
          if ctx == PathParsingContext::Normal {
            self.emit(ExpectedIdentifierInNormalPath {
              span: self.peek().span,
              found: self.peek().kind.clone(),
            });
          } else {
            self.emit(ExpectedSuperOrIdentPath {
              span: self.peek().span,
              found: self.peek().kind.clone(),
            });
          }

          self.advance();
        }
      }
    }

    Path { id: NodeId::new(), root, segments, span: self.span_from(start) }
  }
}
