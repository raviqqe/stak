//! Macros to bundle and use Scheme programs.

use proc_macro::TokenStream;
use proc_macro2::Literal;
use quote::quote;
use std::{env, error::Error, fs::read_to_string, path::Path};
use syn::{parse_macro_input, LitStr};

/// Compiles a program in R7RS Scheme into bytecodes.
///
/// # Examples
///
/// ```rust
/// let bytecodes = stak_macro::compile_r7rs!("(define x 42)");
/// ```
#[proc_macro]
pub fn compile_r7rs(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as LitStr);

    convert_result(generate_r7rs(&input.value()))
}

/// Includes a program in R7RS Scheme as bytecodes.
///
/// # Examples
///
/// ```rust
/// let bytecodes = stak_macro::include_r7rs!("foo.scm");
/// ```
#[proc_macro]
pub fn include_r7rs(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as LitStr);

    convert_result((|| generate_r7rs(&read_file(input)?))())
}

fn generate_r7rs(source: &str) -> Result<TokenStream, Box<dyn Error>> {
    let mut target = vec![];

    stak_compile::compile_r7rs(source, &mut target)?;

    let target = Literal::byte_string(&target);

    Ok(quote! { #target }.into())
}

/// Compiles a program in Scheme into bytecodes with only built-ins.
///
/// # Examples
///
/// ```rust
/// let bytecodes = stak_macro::compile_bare!("($$define x 42)");
/// ```
#[proc_macro]
pub fn compile_bare(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as LitStr);

    convert_result(generate_bare(&input.value()))
}

/// Includes a program in Scheme as bytecodes with only built-ins.
///
/// # Examples
///
/// ```rust
/// let bytecodes = stak_macro::include_bare!("foo.scm");
/// ```
#[proc_macro]
pub fn include_bare(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as LitStr);

    convert_result((|| generate_bare(&read_file(input)?))())
}

fn generate_bare(source: &str) -> Result<TokenStream, Box<dyn Error>> {
    let mut target = vec![];

    stak_compile::compile_bare(source, &mut target)?;

    let target = Literal::byte_string(&target);

    Ok(quote! { #target }.into())
}

fn read_file(path: LitStr) -> Result<String, Box<dyn Error>> {
    Ok(read_to_string(
        Path::new(&env::var("CARGO_MANIFEST_DIR")?)
            .join("src")
            .join(path.value()),
    )?)
}

fn convert_result(result: Result<TokenStream, Box<dyn Error>>) -> TokenStream {
    result.unwrap_or_else(|error| {
        let message = error.to_string();

        quote! { compile_error!(#message) }.into()
    })
}
