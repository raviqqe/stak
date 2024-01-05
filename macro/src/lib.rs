use device::ReadWriteDevice;
use primitive::SmallPrimitiveSet;
use proc_macro::TokenStream;
use proc_macro2::Literal;
use quote::quote;
use std::error::Error;
use syn::{parse_macro_input, LitStr};
use vm::Vm;

const HEAP_SIZE: usize = 1 << 20;
const BUFFER_SIZE: usize = 1 << 20;
const COMPILER_BYTECODES: &[u8] = include_bytes!(std::env!("STAK_BYTECODE_FILE"));

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

    run(input).unwrap_or_else(|error| {
        let message = error.to_string();

        quote! { compile_error!(#message) }.into()
    })
}

fn run(string: LitStr) -> Result<TokenStream, Box<dyn Error>> {
    let mut heap = vec![Default::default(); HEAP_SIZE];
    let device = ReadWriteDevice::new(string.value().as_bytes(), vec![], vec![]);
    let mut vm = Vm::new(&mut heap, SmallPrimitiveSet::new(device))?;

    vm.initialize(COMPILER_BYTECODES.iter().copied())?;

    vm.run()?;

    let bytecodes = Literal::byte_string(vm.primitive_set().device().output());

    Ok(quote! { #bytecodes }.into())
}
