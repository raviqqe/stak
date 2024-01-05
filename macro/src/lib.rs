use device::ReadWriteDevice;
use primitive::SmallPrimitiveSet;
use proc_macro::TokenStream;
use proc_macro2::Literal;
use quote::quote;
use std::error::Error;
use syn::{parse_macro_input, LitStr};
use vm::Vm;

const HEAP_SIZE: usize = 1 << 18;
const COMPILER_BYTECODES: &[u8] = include_bytes!(std::env!("STAK_BYTECODE_FILE"));

/// Compiles a program in R7RS Scheme into bytecodes.
///
/// # Examples
///
/// ```rust
/// let bytecodes = stak_macro::r7rs!("(define x 42)");
/// ```
#[proc_macro]
pub fn r7rs(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as LitStr);

    convert_result(generate_scheme(&input.value()))
}

/// Compiles a program in Scheme into bytecodes with only built-ins but no standard library.
///
/// # Examples
///
/// ```rust
/// let bytecodes = stak_macro::naked!("($$define x 42)");
/// ```
#[proc_macro]
pub fn naked(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as LitStr);

    convert_result(generate_scheme(&input.value()))
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

fn convert_result(result: Result<TokenStream, Box<dyn Error>>) -> TokenStream {
    result.unwrap_or_else(|error| {
        let message = error.to_string();

        quote! { compile_error!(#message) }.into()
    })
}
