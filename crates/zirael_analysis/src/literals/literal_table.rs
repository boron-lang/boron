use dashmap::DashMap;
use num_bigint::BigInt;
use zirael_diagnostics::DiagnosticCtx;
use zirael_parser::NodeId;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FullLiteral {
  Int(BigInt),
  Float(FloatLiteral),
  Bool(bool),
  Char(char),
  Byte(u8),
  String(String),
  Unit,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FloatLiteral {
  pub significand: BigInt,
  pub exponent: i32,
  pub negative: bool,
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
