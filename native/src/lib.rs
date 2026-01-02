//! Stak Scheme primitive sets for optimized primitives of native functions.

#![no_std]

mod arithmetic;
mod equal;
mod list;
mod type_check;

pub use arithmetic::*;
pub use equal::*;
pub use list::*;
pub use type_check::*;
