use proc_macro::TokenStream;
use syn::{parse_macro_input, LitStr};

const bytecodes: &[u8] = include_bytes!(std::env!("STAK_BYTECODE_FILE"));

/// Compiles a program in Scheme into bytecodes.
///
/// # Examples
///
/// ```rust
/// // TBD
/// ```
#[proc_macro]
pub fn scheme(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as LitStr);

    todo!()
}
