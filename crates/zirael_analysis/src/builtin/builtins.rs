use crate::InferTy;
use once_cell::sync::Lazy;
use std::collections::HashMap;
use zirael_hir::expr::ComptimeArg;
use zirael_parser::PrimitiveKind;
use zirael_resolver::prelude::BuiltInKind;
use zirael_utils::prelude::Span;

#[derive(Debug)]
pub struct BuiltInFunction {
  pub kind: BuiltInKind,
  pub return_type: InferTy,
  pub params: Vec<BuiltInParam>,
}

#[derive(Debug)]
pub enum BuiltInParam {
  Expr(InferTy),
  Type,
}

pub static BUILTINS: Lazy<HashMap<BuiltInKind, BuiltInFunction>> = Lazy::new(|| {
  use BuiltInKind::*;

  let mut m = HashMap::new();

  m.insert(
    SizeOf,
    BuiltInFunction {
      kind: SizeOf,
      return_type: InferTy::Primitive(PrimitiveKind::USize, Span::default()),
      params: vec![BuiltInParam::Type],
    },
  );

  m.insert(
    AlignOf,
    BuiltInFunction {
      kind: AlignOf,
      return_type: InferTy::Primitive(PrimitiveKind::USize, Span::default()),
      params: vec![BuiltInParam::Type],
    },
  );

  m
});

pub fn get_builtin(kind: &BuiltInKind) -> &'static BuiltInFunction {
  BUILTINS.get(kind).unwrap()
}
