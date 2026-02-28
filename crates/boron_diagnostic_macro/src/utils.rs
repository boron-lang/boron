use crate::macro_impl::MacroFunctionError;
use syn::{Expr, GenericArgument, Lit, LitStr, PathArguments, Type};

pub fn is_span_type(ty: &Type) -> bool {
  match ty {
    Type::Path(type_path) => {
      type_path.path.segments.last().map(|seg| seg.ident == "Span").unwrap_or(false)
    }
    _ => false,
  }
}

pub fn is_vec_span_type(ty: &Type) -> bool {
  match ty {
    Type::Path(type_path) => {
      let last_seg = type_path.path.segments.last();
      if let Some(seg) = last_seg {
        if seg.ident != "Vec" {
          return false;
        }
        if let PathArguments::AngleBracketed(args) = &seg.arguments {
          if let Some(GenericArgument::Type(inner_ty)) = args.args.first() {
            return is_span_type(inner_ty);
          }
        }
      }
      false
    }
    _ => false,
  }
}

pub fn is_string_type(ty: &Type) -> bool {
  match ty {
    Type::Path(type_path) => {
      type_path.path.segments.last().map(|seg| seg.ident == "String").unwrap_or(false)
    }
    _ => false,
  }
}

pub fn is_vec_string_type(ty: &Type) -> bool {
  match ty {
    Type::Path(type_path) => {
      let last_seg = type_path.path.segments.last();
      if let Some(seg) = last_seg {
        if seg.ident != "Vec" {
          return false;
        }
        if let PathArguments::AngleBracketed(args) = &seg.arguments {
          if let Some(GenericArgument::Type(inner_ty)) = args.args.first() {
            return is_string_type(inner_ty);
          }
        }
      }
      false
    }
    _ => false,
  }
}

pub fn get_lit_str(expr: &Expr) -> Result<LitStr, MacroFunctionError> {
  match expr {
    Expr::Lit(expr_lit) => {
      if let Lit::Str(lit_str) = &expr_lit.lit {
        Ok(lit_str.clone())
      } else {
        Err(MacroFunctionError::InvalidAttribute(
          "Message must be a string literal".to_string(),
        ))
      }
    }
    _ => Err(MacroFunctionError::InvalidAttribute(
      "Message must be a string literal".to_string(),
    )),
  }
}

pub fn is_valid_format_ident(ident: &str) -> bool {
  let ident = ident.strip_prefix("r#").unwrap_or(ident);
  let mut chars = ident.chars();
  let Some(first) = chars.next() else {
    return false;
  };
  if !(first == '_' || first.is_ascii_alphabetic()) {
    return false;
  }
  chars.all(|c| c == '_' || c.is_ascii_alphanumeric())
}
