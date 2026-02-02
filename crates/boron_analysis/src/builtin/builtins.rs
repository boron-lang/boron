use crate::InferTy;
use boron_parser::PrimitiveKind;
use boron_resolver::prelude::BuiltInKind;
use boron_utils::prelude::Span;
use std::collections::HashMap;

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

pub static BUILTINS: std::sync::LazyLock<HashMap<BuiltInKind, BuiltInFunction>> =
  std::sync::LazyLock::new(|| {
    use BuiltInKind::{AlignOf, SizeOf};

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
