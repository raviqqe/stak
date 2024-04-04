//! Macro utilities.

use proc_macro2::TokenStream;
use quote::quote;
use std::{env, error::Error, fs::read_to_string, path::Path};
use syn::LitStr;

pub fn read_source_file(path: LitStr) -> Result<String, Box<dyn Error>> {
    Ok(read_to_string(
        Path::new(&env::var("CARGO_MANIFEST_DIR")?)
            .join("src")
            .join(path.value()),
    )?)
}

pub fn convert_result(result: Result<TokenStream, Box<dyn Error>>) -> TokenStream {
    result.unwrap_or_else(|error| {
        let message = error.to_string();

        quote! { compile_error!(#message) }
    })
}
