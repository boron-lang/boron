use boron_diagnostics::DiagnosticCtx;
use boron_parser::NodeId;
use dashmap::DashMap;
use rustc_apfloat::ieee::{DoubleS, IeeeFloat};

#[derive(Debug, Clone, PartialEq)]
pub enum FullLiteral {
  Int(i128),
  Float(IeeeFloat<DoubleS>),
  Bool(bool),
  Char(char),
  Byte(u8),
  String(String),
  Unit,
}

#[derive(Debug)]
// stores the fully resolved literals
pub struct LiteralTable<'a> {
  literals: DashMap<NodeId, FullLiteral>,
  dcx: &'a DiagnosticCtx,
}

impl<'a> LiteralTable<'a> {
  pub fn new(dcx: &'a DiagnosticCtx) -> Self {
    Self { literals: DashMap::new(), dcx }
  }

  pub fn insert(&self, node: NodeId, lit: FullLiteral) {
    self.literals.insert(node, lit);
  }

  pub fn get(&self, node: NodeId) -> Option<FullLiteral> {
    self.literals.get(&node).map(|v| v.clone())
  }
}
