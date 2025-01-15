//! Stak Scheme primitive sets for optimized primitives of native functions.

#![no_std]

mod dynamic;
mod list;
mod type_check;

pub use dynamic::*;
pub use list::*;
pub use type_check::*;
