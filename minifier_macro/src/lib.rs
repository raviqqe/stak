//! Macros to minify Scheme source codes.

use proc_macro::TokenStream;
use proc_macro2::Literal;
use quote::quote;
use stak_macro_util::convert_result;
use std::{env, error::Error, fs::read_to_string, path::Path, str};
use syn::{parse_macro_input, LitStr};

/// Minifies source codes in Scheme.
///
/// # Examples
///
/// ```rust
/// const PROGRAM: &str = stak_minifier_macro::minify!("( foo  bar )\n\n(baz)");
///
/// assert_eq!(PROGRAM, "(foo bar)\n(baz)\n");
/// ```
#[proc_macro]
pub fn minify(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as LitStr);

    convert_result(minify_source(&input.value())).into()
}

/// Includes and minifies source codes in Scheme in a file.
///
/// # Examples
///
/// ```rust
/// const PROGRAM: &str = stak_minifier_macro::include_minified!("foo.scm");
///
/// assert_eq!(PROGRAM, "(foo bar)\n(baz)\n");
/// ```
#[proc_macro]
pub fn include_minified(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as LitStr);

    convert_result((|| minify_source(&read_file(input)?))()).into()
}

fn minify_source(source: &str) -> Result<proc_macro2::TokenStream, Box<dyn Error>> {
    let mut buffer = vec![];

    stak_minifier::minify(source.as_bytes(), &mut buffer)?;

    let target = Literal::string(str::from_utf8(&buffer)?);

    Ok(quote! { #target }.into())
}

fn read_file(path: LitStr) -> Result<String, Box<dyn Error>> {
    Ok(read_to_string(
        Path::new(&env::var("CARGO_MANIFEST_DIR")?)
            .join("src")
            .join(path.value()),
    )?)
}
