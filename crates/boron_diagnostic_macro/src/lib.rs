mod codes;
mod macro_impl;
mod placeholder;
mod utils;

use crate::macro_impl::macro_derive_impl;
use proc_macro::TokenStream;

#[proc_macro_derive(
  Diagnostic,
  attributes(error, note, notel, warning, help, bug, span, code, help_label)
)]
pub fn derive_diagnostic(item: TokenStream) -> TokenStream {
  macro_derive_impl(item)
}
