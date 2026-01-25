use crate::items::Item;
use crate::parser::Parser;
use crate::parser::items::ITEM_TOKENS;
use crate::{NodeId, ProgramNode, TokenType};

impl Parser<'_> {
  pub fn parse_program(&mut self) -> Option<ProgramNode> {
    self.imports.clear();
    self.discovery_modules.clear();
    let mut items: Vec<Item> = vec![];

    while !self.is_at_end() {
      if let Some(item) = self.parse_item() {
        items.push(item);
      } else {
        self.synchronize_to_next_item();
        self.eat_semis();

        if !self.is_at_end() && !ITEM_TOKENS.contains(&self.peek().kind) {
          self.advance();
        }
      }
    }

    Some(ProgramNode {
      id: NodeId::new(),
      attributes: vec![],
      imports: self.imports.clone(),
      discover_modules: self.discovery_modules.clone(),
      items,
    })
  }

  pub fn check_possible_comment(&mut self) {
    while let TokenType::DocComment(comment) = &self.peek().kind {
      self.push_comment(comment.clone());
      self.advance();
    }
  }
}
