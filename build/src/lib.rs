//! Build scripts for Stak Scheme.

use proc_macro::TokenStream;
use quote::quote;
use stak_macro_util::convert_result;
use std::{env, error::Error, path::Path};
use syn::{parse_macro_input, LitStr};

/// Includes a program in R7RS Scheme as bytecodes.
#[proc_macro]
pub fn include(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as LitStr);

    convert_result(include_result(&input.value())).into()
}

fn include_result(path: &str) -> Result<proc_macro2::TokenStream, Box<dyn Error>> {
    let path = format!(
        "{}",
        Path::new(path)
            .strip_prefix(Path::new(&env::var("CARGO_MANIFEST_DIR")?).join("src"))?
            .display()
    );

    Ok(quote!(include_bytes!(concat!(env!("OUT_DIR"), #path))))
}
