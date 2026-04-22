use proc_macro::TokenStream;
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::{format_ident, quote};
use syn::{
  Attribute, FnArg, Ident, Pat, Result, Token, Type,
  parse::{Parse, ParseStream},
  parse_macro_input,
  punctuated::Punctuated,
  token::Comma,
};

struct QueryItem {
  attrs: Vec<Attribute>,
  cached: bool,
  name: Ident,
  params: Vec<(Ident, Type)>,
  ret_ty: Type,
}

struct QueriesInput {
  items: Vec<QueryItem>,
}

fn is_reference(ty: &Type) -> bool {
  matches!(ty, Type::Reference(_))
}

impl Parse for QueriesInput {
  fn parse(input: ParseStream) -> Result<Self> {
    let mut items = Vec::new();

    while !input.is_empty() {
      let attrs: Vec<Attribute> = input.call(Attribute::parse_outer)?;

      let mut cached = false;
      let forwarded_attrs: Vec<Attribute> = attrs
        .into_iter()
        .filter(|a| {
          if a.path().is_ident("cache") {
            cached = true;
            false
          } else {
            true
          }
        })
        .collect();

      input.parse::<Token![fn]>()?;
      let name: Ident = input.parse()?;

      let content;
      syn::parenthesized!(content in input);
      let raw_params: Punctuated<FnArg, Comma> =
        content.parse_terminated(FnArg::parse, Token![,])?;

      let mut params: Vec<(Ident, Type)> = Vec::new();
      for arg in raw_params {
        match arg {
          FnArg::Typed(pt) => {
            let param_name = match *pt.pat {
              Pat::Ident(pi) => pi.ident,
              _ => {
                return Err(syn::Error::new(
                  Span::call_site(),
                  "expected identifier in query parameter",
                ));
              }
            };
            params.push((param_name, *pt.ty));
          }
          FnArg::Receiver(_) => {
            return Err(syn::Error::new(
              Span::call_site(),
              "`self` not allowed in query parameters",
            ));
          }
        }
      }

      let ret_ty: Type = if input.peek(Token![:]) {
        input.parse::<Token![:]>()?;
        input.parse()?
      } else {
        syn::parse_quote! { () }
      };

      input.parse::<Token![;]>()?;

      if cached && is_reference(&ret_ty) {
        return Err(syn::Error::new_spanned(
          &ret_ty,
          "#[cache] cannot be used with a reference return type; \
                     the cache owns its values and cannot hand out references into it",
        ));
      }

      items.push(QueryItem { attrs: forwarded_attrs, cached, name, params, ret_ty });
    }

    Ok(QueriesInput { items })
  }
}

#[proc_macro]
pub fn queries(input: TokenStream) -> TokenStream {
  let QueriesInput { items } = parse_macro_input!(input as QueriesInput);

  let mut field_defs = Vec::<TokenStream2>::new();
  let mut method_impls = Vec::<TokenStream2>::new();

  for item in &items {
    let QueryItem { attrs, cached, name, params, ret_ty } = item;

    let param_tys: Vec<&Type> = params.iter().map(|(_, ty)| ty).collect();
    let param_names: Vec<&Ident> = params.iter().map(|(n, _)| n).collect();

    field_defs.push(quote! {
        #(#attrs)*
        pub #name: Option<fn(&'ctx BCtx<'ctx>, (#(#param_tys,)*)) -> #ret_ty>,
    });

    if *cached {
      let cache_name = format_ident!("{}_cache", name);
      field_defs.push(quote! {
          pub #cache_name: std::cell::RefCell<
              std::collections::HashMap<(#(#param_tys,)*), #ret_ty>
          >,
      });
    }

    let expect_msg =
      format!("attempted to call {} query without its implementation", name);

    let method = if *cached {
      let cache_name = format_ident!("{}_cache", name);
      quote! {
          #(#attrs)*
          pub fn #name(&'ctx self, #(#param_names: #param_tys),*) -> #ret_ty {
              let params = (#(#param_names,)*);
              if let Some(cached) = self.queries.#cache_name.borrow().get(&params) {
                  return cached.clone();
              }
              let f = self.queries.#name.expect(#expect_msg);
              let result = f(self, params.clone());
              self.queries.#cache_name.borrow_mut().insert(params, result.clone());
              result
          }
      }
    } else {
      quote! {
          #(#attrs)*
          pub fn #name(&'ctx self, #(#param_names: #param_tys),*) -> #ret_ty {
              (self.queries.#name.expect(#expect_msg))(self, (#(#param_names,)*))
          }
      }
    };

    method_impls.push(method);
  }

  quote! {
      #[derive(Debug, Default)]
      pub struct Queries<'ctx> {
          #(#field_defs)*
      }

      impl<'ctx> BCtx<'ctx> {
          #(#method_impls)*
      }
  }
  .into()
}
