use crate::codes::collect_struct_code;
use crate::utils::{
  get_lit_str, is_span_type, is_vec_span_type, is_vec_string_type,
};
use proc_macro::TokenStream;
use proc_macro2::{Ident, Span};
use quote::quote;
use std::collections::HashMap;
use syn::{
  parse_macro_input, spanned::Spanned, Data, DeriveInput, Expr, Fields, FieldsNamed,
};
use thiserror::Error;
use crate::placeholder::extract_named_placeholders;

#[derive(Debug, Error)]
pub enum MacroFunctionError {
  #[error("Parse error: {0}")]
  ParseError(#[from] syn::Error),
  #[error("Invalid attribute: {0}")]
  InvalidAttribute(String),
  #[error("Missing required attribute")]
  MissingAttribute,
}

const VALID_SEVERITIES: &[&str] = &["help", "warning", "error", "bug", "note"];
const VALID_LABEL_SEVERITIES: &[&str] = &["error", "warning", "help_label"];

fn collect_struct_messages(
  ast: &DeriveInput,
  attr_name: &str,
) -> Result<Vec<syn::LitStr>, MacroFunctionError> {
  let mut out = Vec::new();

  for attr in &ast.attrs {
    let is_match =
      attr.path().segments.last().map(|seg| seg.ident == attr_name).unwrap_or(false);

    if !is_match {
      continue;
    }

    let expr = attr.parse_args::<Expr>()?;
    out.push(get_lit_str(&expr)?);
  }

  Ok(out)
}

fn get_string_literal(expr: &Expr) -> Result<String, MacroFunctionError> {
  Ok(get_lit_str(expr)?.value())
}

fn is_valid_severity(ident: &str) -> bool {
  VALID_SEVERITIES.contains(&ident.to_lowercase().as_str())
}

fn is_valid_label_severity(ident: &str) -> bool {
  VALID_LABEL_SEVERITIES.contains(&ident.to_lowercase().as_str())
}

fn get_diagnostic_level(severity: &str) -> proc_macro2::TokenStream {
  match severity.to_lowercase().as_str() {
    "error" => quote! { boron_diagnostics::DiagnosticLevel::Error },
    "warning" => quote! { boron_diagnostics::DiagnosticLevel::Warning },
    "bug" => quote! { boron_diagnostics::DiagnosticLevel::Bug },
    "help_label" => quote! { boron_diagnostics::DiagnosticLevel::Help },
    _ => quote! { boron_diagnostics::DiagnosticLevel::Error },
  }
}

pub fn get_side_fields<'a>(
  fields: &'a FieldsNamed,
  name: &str,
) -> Vec<(&'a syn::Ident, bool)> {
  fields
    .named
    .iter()
    .filter_map(|field| {
      let has_field = field.attrs.iter().any(|attr| {
        attr.path().segments.last().map(|seg| seg.ident == name).unwrap_or(false)
      });

      if has_field {
        let is_vec = is_vec_string_type(&field.ty);
        field.ident.as_ref().map(|ident| (ident, is_vec))
      } else {
        None
      }
    })
    .collect()
}

pub fn macro_derive_impl(item: TokenStream) -> TokenStream {
  let ast = parse_macro_input!(item as DeriveInput);

  match impl_diagnostic_derive(&ast) {
    Ok(token_stream) => token_stream,
    Err(error) => {
      TokenStream::from(syn::Error::new(ast.span(), error.to_string()).to_compile_error())
    }
  }
}

fn impl_diagnostic_derive(ast: &DeriveInput) -> Result<TokenStream, MacroFunctionError> {
  let struct_name = &ast.ident;

  let struct_note_messages = collect_struct_messages(ast, "note")?;
  let struct_help_messages = collect_struct_messages(ast, "help")?;
  let struct_code = collect_struct_code(ast)?;

  let diagnostic_attr = ast
    .attrs
    .iter()
    .find(|attr| {
      attr
        .path()
        .segments
        .last()
        .map(|seg| {
          let ident = seg.ident.to_string();
          ident == "error" || ident == "warning" || ident == "bug"
        })
        .unwrap_or(false)
    })
    .ok_or(MacroFunctionError::MissingAttribute)?;

  let severity =
    diagnostic_attr.path().segments.last().unwrap().ident.to_string().to_lowercase();
  let error_message_lit = get_lit_str(&diagnostic_attr.parse_args::<Expr>()?)?;
  let error_message = error_message_lit.value();

  let fields = match &ast.data {
    Data::Struct(data) => match &data.fields {
      Fields::Named(fields) => fields,
      _ => {
        return Err(MacroFunctionError::InvalidAttribute(
          "Only named fields are supported".to_string(),
        ));
      }
    },
    _ => {
      return Err(MacroFunctionError::InvalidAttribute(
        "Only structs are supported".to_string(),
      ));
    }
  };

  // Fields without severity attributes (used in message interpolation)
  let message_fields: Vec<_> = fields
    .named
    .iter()
    .filter_map(|field| {
      let has_severity_attr = field.attrs.iter().any(|attr| {
        attr
          .path()
          .segments
          .last()
          .map(|seg| is_valid_severity(&seg.ident.to_string()))
          .unwrap_or(false)
      });

      if !has_severity_attr { field.ident.as_ref() } else { None }
    })
    .collect();

  let message_field_map: HashMap<String, &Ident> =
    message_fields.iter().map(|&ident| (ident.to_string(), ident)).collect();

  let main_message_used_names = extract_named_placeholders(&error_message)?;
  let mut main_message_used_fields: Vec<&Ident> = Vec::new();
  for name in main_message_used_names {
    let Some(ident) = message_field_map.get(&name) else {
      return Err(MacroFunctionError::InvalidAttribute(format!(
        "unknown format placeholder `{{{}}}` in #[{}(...)]",
        name, severity
      )));
    };
    main_message_used_fields.push(*ident);
  }

  let label_fields = get_label_fields(fields);
  let note_fields = get_side_fields(fields, "note");
  let help_fields = get_side_fields(fields, "help");

  let main_message_field_refs = main_message_used_fields.iter().map(|&ident| {
    quote! { let #ident = &self.#ident; }
  });

  let mut label_implementations: Vec<proc_macro2::TokenStream> = Vec::new();
  for (ident, message, severity, is_vec) in &label_fields {
    let diagnostic_level = get_diagnostic_level(severity);

    if message.is_empty() {
      if *is_vec {
        label_implementations.push(quote! {
          self.#ident.iter().map(|span| {
            boron_diagnostics::Label::new(String::new(), *span, #diagnostic_level)
          })
        });
      } else {
        label_implementations.push(quote! {
          std::iter::once(boron_diagnostics::Label::new(String::new(), self.#ident, #diagnostic_level))
        });
      }
      continue;
    }

    let message_lit = syn::LitStr::new(message, Span::call_site());
    let used_names = extract_named_placeholders(message)?;
    let mut used_fields: Vec<&syn::Ident> = Vec::new();
    for name in used_names {
      let Some(field_ident) = message_field_map.get(&name) else {
        return Err(MacroFunctionError::InvalidAttribute(format!(
          "unknown format placeholder `{{{}}}` in label message",
          name
        )));
      };
      used_fields.push(*field_ident);
    }

    if used_fields.is_empty() {
      if *is_vec {
        label_implementations.push(quote! {
          {
            let message = format!(#message_lit);
            self.#ident.iter().map(move |span| {
              boron_diagnostics::Label::new(message.clone(), *span, #diagnostic_level)
            })
          }
        });
      } else {
        label_implementations.push(quote! {
          {
            let message = format!(#message_lit);
            std::iter::once(boron_diagnostics::Label::new(message, self.#ident, #diagnostic_level))
          }
        });
      }
    } else {
      let field_refs = used_fields.iter().map(|&field_ident| {
        quote! { let #field_ident = &self.#field_ident; }
      });
      let field_args = used_fields.iter().map(|&field_ident| {
        quote! { #field_ident = #field_ident }
      });
      if *is_vec {
        label_implementations.push(quote! {
          {
            #(#field_refs)*
            let message = format!(#message_lit, #(#field_args),*);
            self.#ident.iter().map(move |span| {
              boron_diagnostics::Label::new(message.clone(), *span, #diagnostic_level)
            })
          }
        });
      } else {
        label_implementations.push(quote! {
          {
            #(#field_refs)*
            let message = format!(#message_lit, #(#field_args),*);
            std::iter::once(boron_diagnostics::Label::new(message, self.#ident, #diagnostic_level))
          }
        });
      }
    }
  }

  let note_field_refs = note_fields.iter().map(|(ident, _)| {
    quote! { let #ident = &self.#ident; }
  });

  let note_strings = note_fields.iter().map(|(ident, is_vec)| {
    if *is_vec {
      quote! { #ident.clone() }
    } else {
      quote! { vec![#ident.to_string()] }
    }
  });

  let help_field_refs = help_fields.iter().map(|(ident, _)| {
    quote! { let #ident = &self.#ident; }
  });

  let help_strings = help_fields.iter().map(|(ident, is_vec)| {
    if *is_vec {
      quote! { #ident.clone() }
    } else {
      quote! { vec![#ident.to_string()] }
    }
  });

  let struct_note_pushes = struct_note_messages.iter().map(|lit| {
    quote! { notes.push(#lit.to_string()); }
  });

  let struct_help_pushes = struct_help_messages.iter().map(|lit| {
    quote! { helps.push(#lit.to_string()); }
  });

  let diagnostic_level = get_diagnostic_level(&severity);

  let notes_impl = if note_fields.is_empty() && struct_note_messages.is_empty() {
    quote! { Vec::new() }
  } else {
    quote! {
      {
        #(#note_field_refs)*
        let mut notes = Vec::new();
        #(#struct_note_pushes)*
        #(notes.extend(#note_strings);)*
        notes
      }
    }
  };

  let helps_impl = if help_fields.is_empty() && struct_help_messages.is_empty() {
    quote! { Vec::new() }
  } else {
    quote! {
      {
        #(#help_field_refs)*
        let mut helps = Vec::new();
        #(#struct_help_pushes)*
        #(helps.extend(#help_strings);)*
        helps
      }
    }
  };

  let message_impl = if main_message_used_fields.is_empty() {
    quote! {
      let message = format!(#error_message_lit);
    }
  } else {
    let field_args = main_message_used_fields.iter().map(|&ident| {
      quote! { #ident = #ident }
    });
    quote! {
      #(#main_message_field_refs)*
      let message = format!(#error_message_lit, #(#field_args),*);
    }
  };

  let code_impl = if let Some(code_expr) = struct_code {
    quote! { Some(#code_expr) }
  } else {
    quote! { None }
  };

  let labels_impl = if label_implementations.is_empty() {
    quote! { vec![] }
  } else {
    quote! {
      vec![
        #(#label_implementations),*
      ].into_iter().flatten().collect()
    }
  };

  Ok(TokenStream::from(quote! {
      #[automatically_derived]
        impl boron_diagnostics::ToDiagnostic for #struct_name {
          fn to_diagnostic(self) -> boron_diagnostics::Diag {
            #message_impl

            boron_diagnostics::Diag {
                  message,
                  level: #diagnostic_level,
                  labels: #labels_impl,
                  notes: #notes_impl,
                helps: #helps_impl,
                code: #code_impl,
              }
          }
      }
  }))
}

fn get_label_fields(fields: &FieldsNamed) -> Vec<(Ident, String, String, bool)> {
  let label_fields: Vec<_> = fields
    .named
    .iter()
    .filter_map(|field| {
      let is_span = is_span_type(&field.ty);
      let is_vec_span = is_vec_span_type(&field.ty);

      if !is_span && !is_vec_span {
        return None;
      }

      let attr = field.attrs.iter().find(|attr| {
        attr
          .path()
          .segments
          .last()
          .map(|seg| is_valid_label_severity(&seg.ident.to_string()))
          .unwrap_or(false)
      })?;

      let field_ident = field.ident.as_ref()?;
      let severity =
        attr.path().segments.last().unwrap().ident.to_string().to_lowercase();

      let message = attr
        .parse_args::<Expr>()
        .ok()
        .and_then(|expr| get_string_literal(&expr).ok())
        .unwrap_or_default();

      Some((field_ident.clone(), message, severity, is_vec_span))
    })
    .collect();
  label_fields
}
