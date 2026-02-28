use crate::macro_impl::MacroFunctionError;
use proc_macro2::TokenStream;
use quote::quote;
use syn::{DeriveInput, Expr, Meta};

pub fn parse_code_to_tokens(expr: &Expr) -> Result<TokenStream, MacroFunctionError> {
  match expr {
    Expr::Path(path) => {
      let segments = &path.path.segments;
      if segments.len() == 1 {
        let ident = &segments[0].ident;
        Ok(quote! { boron_diagnostics::codes::#ident })
      } else {
        Ok(quote! { #path })
      }
    }

    _ => Err(MacroFunctionError::InvalidAttribute(
      "code must be an integer literal or a path (e.g. LEX_FOO)".to_string(),
    )),
  }
}

pub fn collect_struct_code(
  ast: &DeriveInput,
) -> Result<Option<TokenStream>, MacroFunctionError> {
  let mut out: Option<TokenStream> = None;

  for attr in &ast.attrs {
    let is_match =
      attr.path().segments.last().map(|seg| seg.ident == "code").unwrap_or(false);
    if !is_match {
      continue;
    }

    if out.is_some() {
      return Err(MacroFunctionError::InvalidAttribute(
        "duplicate #[code] attribute".to_string(),
      ));
    }

    let code_expr: Expr = match &attr.meta {
      Meta::List(_) => attr.parse_args::<Expr>()?,
      Meta::NameValue(nv) => nv.value.clone(),
      Meta::Path(_) => {
        return Err(MacroFunctionError::InvalidAttribute(
          "#[code] requires a value like #[code(1234)]".to_string(),
        ));
      }
    };

    out = Some(parse_code_to_tokens(&code_expr)?);
  }

  Ok(out)
}
