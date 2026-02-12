use crate::expressions::Expr;
use crate::items::Item;
use crate::parser::Parser;
use crate::parser::errors::{
  ABIMustBeExplicit, ComptimeExternTogether, ConstCannotBeUninitialized,
  ConstExpectedFuncOrIdent, ConstItemsNeedTypeAnnotation, InvalidAbi, ModStringLit,
};
use crate::{
  ConstItem, FunctionModifiers, ItemKind, ModItem, NodeId, Path, PathParsingContext,
  TokenType, Type, Visibility, log_parse_failure,
};
use boron_session::prelude::debug;
use boron_source::prelude::Span;
use boron_target::abi::ABI;

mod functions;
mod import;
mod structs;

pub const ITEM_TOKENS: &[TokenType] = &[
  TokenType::Pub,
  TokenType::Mod,
  TokenType::Const,
  TokenType::Func,
  TokenType::Import,
  TokenType::Comptime,
  TokenType::Struct,
  TokenType::Extern,
];

// tokens allowed to start a new item inside a struct
pub const STRUCT_ITEM_TOKENS: &[TokenType] =
  &[TokenType::Pub, TokenType::Const, TokenType::Func, TokenType::Comptime];

impl Parser<'_> {
  pub fn is_module_discovery_beginning(&self) -> bool {
    matches!(
      self.peek().kind,
      TokenType::Identifier(_)
        | TokenType::SelfValue
        | TokenType::Package
        | TokenType::Super
        | TokenType::StringLiteral(_) // the string is here only because we report an error later
    )
  }

  fn parse_module_discovery(&mut self) -> Option<Path> {
    if let TokenType::StringLiteral(_) = &self.peek().kind {
      self.emit(ModStringLit { span: self.peek().span });

      return None;
    }
    Some(self.parse_path(PathParsingContext::ImportOrMod))
  }

  pub(crate) fn parse_item(&mut self) -> Option<Item> {
    self.doc_comment = None;
    self.check_possible_comment();

    let span_start = self.peek().span;
    let visibility = if let Some(t) = self.is(TokenType::Pub) {
      Visibility::Public(t.span)
    } else {
      Visibility::Private
    };

    let token = self.expect_any(ITEM_TOKENS, "as an item beginning")?.kind;
    let kind = match token {
      TokenType::Mod => {
        if self.is_identifier() && self.peek_ahead(1)?.kind == TokenType::LeftBrace {
          let name = self.parse_identifier();
          self.expect(TokenType::LeftBrace, "to open a module declaration");

          todo!("mod decl not implemented");
          Some(ItemKind::Mod(ModItem {
            name,
            id: NodeId::new(),
            span: Default::default(),
            items: vec![],
          }))
        } else if self.is_module_discovery_beginning() {
          let path = self.parse_module_discovery();
          let path = path?;

          self.discovery_modules.push(path);
          self.eat_semis();
          return None;
        } else {
          None
        }
      }
      token @ (TokenType::Comptime | TokenType::Extern) => {
        let modifiers = self.parse_function_modifiers(token);
        let span = self.peek().span;
        if self.peek().kind == TokenType::Func {
          self.eat(TokenType::Func);

          Some(ItemKind::Function(log_parse_failure!(
            self.parse_function(modifiers, span),
            "module item"
          )?))
        } else {
          self.expect(TokenType::Func, "after modifiers only `func` is a valid keyword");
          self.synchronize_to_next_item();
          None
        }
      }
      TokenType::Const => {
        log_parse_failure!(self.parse_const(span_start), "const item")
      }
      TokenType::Func => Some(ItemKind::Function(log_parse_failure!(
        self.parse_function(FunctionModifiers::empty(), span_start),
        "function item"
      )?)),
      TokenType::Import => {
        let import = self.parse_import();

        if let Some(import) = import {
          self.imports.push(import);
        } else {
          debug!("couldn't parse import");
        }

        return None;
      }
      TokenType::Struct => Some(ItemKind::Struct(log_parse_failure!(
        self.parse_struct(span_start),
        "struct item"
      )?)),
      _ => unreachable!(),
    };

    self.eat_semis();

    let kind = kind?;

    Some(Item {
      id: kind.node_id(),
      kind,
      // TODO: attributes parsing
      attributes: vec![],
      span: self.span_from(span_start),
      visibility,
      doc_comments: self.doc_comment.clone(),
    })
  }

  /// tries to find next item to start parsing from.
  pub fn synchronize_to_next_item(&mut self) {
    self.advance_until_one_of(ITEM_TOKENS);
  }

  fn parse_function_modifiers(&mut self, first: TokenType) -> FunctionModifiers {
    let mut comptime = false;
    let mut external = None;

    match first {
      TokenType::Comptime => {
        comptime = true;
        if self.eat(TokenType::Extern) {
          self.emit(ComptimeExternTogether { span: self.previous().span });
          external = self.parse_extern_abi();
        }
      }
      TokenType::Extern => {
        external = self.parse_extern_abi();
        if self.eat(TokenType::Comptime) {
          self.emit(ComptimeExternTogether { span: self.previous().span });
          comptime = true;
        }
      }
      _ => {}
    }

    FunctionModifiers { comptime, external }
  }

  fn parse_extern_abi(&mut self) -> Option<ABI> {
    let abi_span = self.peek().span;
    let abi = if let TokenType::StringLiteral(string) = self.peek().kind.clone() {
      self.advance();
      Some(string)
    } else {
      self.emit(ABIMustBeExplicit { span: abi_span });
      None
    };

    if let Some(abi_str) = abi {
      let abi = ABI::parse_from_string(abi_str.clone());
      if abi.is_none() {
        self.emit(InvalidAbi { abi: abi_str, span: abi_span });
      }
      abi
    } else {
      None
    }
  }

  fn parse_const(&mut self, span: Span) -> Option<ItemKind> {
    if let TokenType::Identifier(_) = self.peek().kind {
      let ident = self.parse_identifier();

      let colon = self.eat(TokenType::Colon);
      let ty = if !colon {
        self.emit(ConstItemsNeedTypeAnnotation { span: self.previous().span });
        Type::Invalid
      } else {
        let ty = self.parse_type();
        if matches!(ty, Type::Invalid) {
          self.emit(ConstItemsNeedTypeAnnotation { span: self.previous().span });
        }

        ty
      };

      let expr = if !self.eat(TokenType::Assign) {
        self.emit(ConstCannotBeUninitialized { span: self.peek().span });
        Expr::dummy()
      } else {
        self.parse_expr()
      };

      Some(ItemKind::Const(ConstItem {
        id: NodeId::new(),
        name: ident,
        ty,
        value: expr,
        span: self.span_from(span),
      }))
    } else {
      self.emit(ConstExpectedFuncOrIdent {
        span: self.peek().span,
        found: self.peek().kind.clone(),
      });

      None
    }
  }
}
