use crate::InferTy;
use boron_parser::{Mutability, PrimitiveKind};
use boron_resolver::prelude::BuiltInKind;
use boron_session::prelude::Span;
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
    use BuiltInKind::{AlignOf, Os, SizeOf};

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

    m.insert(
      Os,
      BuiltInFunction {
        kind: Os,
        return_type: InferTy::Ptr {
          mutability: Mutability::Const,
          ty: Box::new(InferTy::Primitive(PrimitiveKind::U8, Span::default())),
          span: Span::default(),
        },
        params: vec![],
      },
    );

    m
  });

pub fn get_builtin(kind: &BuiltInKind) -> &'static BuiltInFunction {
  BUILTINS.get(kind).unwrap()
}
