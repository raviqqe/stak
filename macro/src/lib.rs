use device::ReadWriteDevice;
use primitive::SmallPrimitiveSet;
use proc_macro::TokenStream;
use proc_macro2::Literal;
use quote::quote;
use std::{env, error::Error, fs::read_to_string, path::Path};
use syn::{parse_macro_input, LitStr};
use vm::Vm;

const HEAP_SIZE: usize = 1 << 20;
const PRELUDE_SOURCE: &str = include_str!("prelude.scm");
const COMPILER_BYTECODES: &[u8] = include_bytes!(std::env!("STAK_BYTECODE_FILE"));

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

    convert_result(generate_scheme(
        &(PRELUDE_SOURCE.to_owned() + &input.value()),
    ))
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

    convert_result((|| {
        generate_scheme(&(PRELUDE_SOURCE.to_owned() + &read_file(input)?))
    })())
}

/// Compiles a program in Scheme into bytecodes with only built-ins.
///
/// # Examples
///
/// ```rust
/// let bytecodes = stak_macro::compile_naked!("($$define x 42)");
/// ```
#[proc_macro]
pub fn compile_naked(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as LitStr);

    convert_result(generate_scheme(&input.value()))
}

/// Includes a program in Scheme as bytecodes with only built-ins.
///
/// # Examples
///
/// ```rust
/// let bytecodes = stak_macro::include_naked!("foo.scm");
/// ```
#[proc_macro]
pub fn include_naked(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as LitStr);

    convert_result((|| generate_scheme(&read_file(input)?))())
}

fn generate_scheme(source: &str) -> Result<TokenStream, Box<dyn Error>> {
    let mut heap = vec![Default::default(); HEAP_SIZE];
    let device = ReadWriteDevice::new(source.as_bytes(), vec![], vec![]);
    let mut vm = Vm::new(&mut heap, SmallPrimitiveSet::new(device))?;

    vm.initialize(COMPILER_BYTECODES.iter().copied())?;

    vm.run()?;

    let bytecodes = Literal::byte_string(vm.primitive_set().device().output());

    Ok(quote! { #bytecodes }.into())
}

fn read_file(path: LitStr) -> Result<String, Box<dyn Error>> {
    Ok(read_to_string(
        &Path::new(&env::var("CARGO_MANIFEST_DIR")?).join(&path.value()),
    )?)
}

fn convert_result(result: Result<TokenStream, Box<dyn Error>>) -> TokenStream {
    result.unwrap_or_else(|error| {
        let message = error.to_string();

        quote! { compile_error!(#message) }.into()
    })
}
