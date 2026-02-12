use crate::ids::HirId;

use crate::{Expr, Literal};
use boron_resolver::DefId;
use boron_session::prelude::Span;
use boron_source::ident_table::Identifier;

#[derive(Debug, Clone)]
pub struct Pat {
  pub hir_id: HirId,
  pub kind: PatKind,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub enum PatKind {
  /// Wildcard: `_`
  Wild,

  /// Binding: `x`, `mut x`
  Binding {
    def_id: DefId,
    name: Identifier,
    is_mut: bool,
    /// `x @ Some(_)`
    subpat: Option<Box<Pat>>,
  },

  /// `42`, `"hello"`, `true`
  Literal(Literal),

  /// Tuple pattern: `(a, b, c)`
  Tuple(Vec<Pat>),

  /// Struct pattern: `Point { x, y }`
  Struct {
    def_id: DefId,
    fields: Vec<FieldPat>,
    rest: bool,
  },

  /// Tuple struct / enum variant pattern: `Some(x)`, `None`
  TupleStruct {
    def_id: DefId,
    pats: Vec<Pat>,
  },

  /// Path pattern (unit variant, const): `None`, `CONST_VALUE`
  Path {
    def_id: DefId,
  },

  /// `A | B | C`
  Or(Vec<Pat>),

  /// `[first, .., last]`
  Slice {
    prefix: Vec<Pat>,
    middle: Option<Box<Pat>>,
    suffix: Vec<Pat>,
  },

  /// `1..=10`
  Range {
    start: Option<Box<Expr>>,
    end: Option<Box<Expr>>,
    inclusive: bool,
  },

  Err,
}

#[derive(Debug, Clone)]
pub struct FieldPat {
  pub hir_id: HirId,
  pub name: Identifier,
  pub pat: Pat,
  pub span: Span,
}
