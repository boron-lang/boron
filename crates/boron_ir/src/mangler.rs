use boron_hir::{Hir, SemanticTy};
use boron_parser::{Mutability, PrimitiveKind};
use boron_resolver::DefId;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MangledSymbolKey {
  pub def_id: DefId,
  pub type_args: Vec<SemanticTy>,
}

impl MangledSymbolKey {
  pub fn new(def_id: DefId, type_args: Vec<SemanticTy>) -> Self {
    Self { def_id, type_args }
  }

  pub fn mono(def_id: DefId) -> Self {
    Self { def_id, type_args: vec![] }
  }
}

#[derive(Debug)]
pub struct SymbolMangler<'a> {
  hir: &'a Hir,
  cache: HashMap<MangledSymbolKey, String>,
}

impl<'a> SymbolMangler<'a> {
  pub fn new(hir: &'a Hir) -> Self {
    Self { hir, cache: HashMap::new() }
  }

  pub fn mangle_struct(&mut self, def_id: DefId, type_args: &[SemanticTy]) -> String {
    let key = MangledSymbolKey::new(def_id, type_args.to_vec());

    if let Some(name) = self.cache.get(&key) {
      return name.clone();
    }

    let base_name = self
      .hir
      .get_struct(def_id)
      .map(|s| s.name.text())
      .unwrap_or_else(|| format!("struct_{}", def_id.index()));

    let mangled = self.mangle_with_type_args(&base_name, type_args);
    self.cache.insert(key, mangled.clone());
    mangled
  }

  pub fn mangle_function(&mut self, def_id: DefId, type_args: &[SemanticTy]) -> String {
    let key = MangledSymbolKey::new(def_id, type_args.to_vec());

    if let Some(name) = self.cache.get(&key) {
      return name.clone();
    }

    let base_name = self
      .hir
      .get_function(def_id)
      .map(|f| f.name.text())
      .unwrap_or_else(|| format!("fn_{}", def_id.index()));

    let mangled = self.mangle_with_type_args(&base_name, type_args);
    self.cache.insert(key, mangled.clone());
    mangled
  }

  pub fn mangle_enum(&mut self, def_id: DefId, type_args: &[SemanticTy]) -> String {
    let key = MangledSymbolKey::new(def_id, type_args.to_vec());

    if let Some(name) = self.cache.get(&key) {
      return name.clone();
    }

    let base_name = self
      .hir
      .get_enum(def_id)
      .map(|e| e.name.text())
      .unwrap_or_else(|| format!("enum_{}", def_id.index()));

    let mangled = self.mangle_with_type_args(&base_name, type_args);
    self.cache.insert(key, mangled.clone());
    mangled
  }

  pub fn get(&self, def_id: DefId, type_args: &[SemanticTy]) -> Option<&String> {
    let key = MangledSymbolKey::new(def_id, type_args.to_vec());
    self.cache.get(&key)
  }

  pub fn get_by_key(&self, key: &MangledSymbolKey) -> Option<&String> {
    self.cache.get(key)
  }

  pub fn iter(&self) -> impl Iterator<Item = (&MangledSymbolKey, &String)> {
    self.cache.iter()
  }

  fn mangle_with_type_args(&self, base_name: &str, type_args: &[SemanticTy]) -> String {
    if type_args.is_empty() {
      return base_name.to_owned();
    }

    let type_suffix: Vec<String> =
      type_args.iter().map(|ty| self.mangle_type(ty)).collect();

    format!("{}${}", base_name, type_suffix.join("$"))
  }

  fn mangle_type(&self, ty: &SemanticTy) -> String {
    match ty {
      SemanticTy::Primitive(kind) => self.mangle_primitive(*kind),

      SemanticTy::Struct { def_id, fields } => {
        let base = self
          .hir
          .get_struct(*def_id)
          .map(|s| s.name.text())
          .unwrap_or_else(|| format!("s{}", def_id.index()));

        if fields.is_empty() {
          base
        } else {
          let field_strs: Vec<String> =
            fields.iter().map(|f| self.mangle_type(f)).collect();
          format!("{}${}", base, field_strs.join("$"))
        }
      }

      SemanticTy::Enum { def_id, .. } => self
        .hir
        .get_enum(*def_id)
        .map(|e| e.name.text())
        .unwrap_or_else(|| format!("e{}", def_id.index())),

      SemanticTy::Ptr { mutability, inner } => {
        let mut_str = match mutability {
          Mutability::Mut => "m",
          Mutability::Const => "c",
        };
        format!("p{}${}", mut_str, self.mangle_type(inner))
      }

      SemanticTy::Optional(inner) => {
        format!("o${}", self.mangle_type(inner))
      }

      SemanticTy::Array { elem, len } => {
        format!("a${}${}", self.mangle_type(elem), len)
      }

      SemanticTy::Slice(inner) => {
        format!("sl${}", self.mangle_type(inner))
      }

      SemanticTy::Tuple(tys) => {
        if tys.is_empty() {
          "t0".to_owned()
        } else {
          let ty_strs: Vec<String> = tys.iter().map(|t| self.mangle_type(t)).collect();
          format!("t{}${}", tys.len(), ty_strs.join("$"))
        }
      }

      SemanticTy::Fn { params, ret } => {
        let param_strs: Vec<String> =
          params.iter().map(|p| self.mangle_type(p)).collect();
        let ret_str = self.mangle_type(ret);
        format!("f${}${}", param_strs.join("$"), ret_str)
      }

      SemanticTy::Unit => "u".to_owned(),
      SemanticTy::Never => "n".to_owned(),
      SemanticTy::Error => "err".to_owned(),
    }
  }

  fn mangle_primitive(&self, kind: PrimitiveKind) -> String {
    match kind {
      PrimitiveKind::I8 => "i8".to_owned(),
      PrimitiveKind::I16 => "i16".to_owned(),
      PrimitiveKind::I32 => "i32".to_owned(),
      PrimitiveKind::I64 => "i64".to_owned(),
      PrimitiveKind::I128 => "i128".to_owned(),
      PrimitiveKind::ISize => "isize".to_owned(),
      PrimitiveKind::U8 => "u8".to_owned(),
      PrimitiveKind::U16 => "u16".to_owned(),
      PrimitiveKind::U32 => "u32".to_owned(),
      PrimitiveKind::U64 => "u64".to_owned(),
      PrimitiveKind::U128 => "u128".to_owned(),
      PrimitiveKind::USize => "usize".to_owned(),
      PrimitiveKind::F32 => "f32".to_owned(),
      PrimitiveKind::F64 => "f64".to_owned(),
      PrimitiveKind::Bool => "bool".to_owned(),
      PrimitiveKind::Char => "char".to_owned(),
    }
  }
}
