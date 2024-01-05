use proc_macro::TokenStream;
use syn::{parse_macro_input, LitStr};

/// Compiles a program in Scheme into bytecodes.
///
/// # Examples
///
/// ```rust
/// melior::dialect! {
///     name: "func",
///     table_gen: r#"include "mlir/Dialect/Func/IR/FuncOps.td""#
/// }
/// ```
#[proc_macro]
pub fn scheme(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as LitStr);

    todo!()
}
