//! Build scripts for Stak Scheme.

use proc_macro::TokenStream;
use proc_macro2::Literal;
use quote::quote;
use stak_compiler::CompileError;
use stak_macro_util::{convert_result, read_source_file};
use std::error::Error;
use syn::{parse_macro_input, LitStr};

/// Includes a program in R7RS Scheme as bytecodes.
#[proc_macro]
pub fn include(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as LitStr);

    let foo = env::var("CARGO_MANIFEST_DIR")?;

    quote!(include_bytes!(concat!(env!("OUT_DIR"), #foo)))
}

fn include_result(path: &str) -> Result<TokenStream, Box<dyn Error>> {
    let manifest_dir = env::var("CARGO_MANIFEST_DIR")?;

    quote!(include_bytes!(concat!(env!("OUT_DIR"), #foo)))
}
