//! Stak Scheme primitive sets for optimized primitives of native functions.

#![no_std]

#[cfg(feature = "alloc")]
extern crate alloc;

#[cfg(feature = "alloc")]
mod dynamic;
mod list;
mod type_check;

#[cfg(feature = "alloc")]
pub use dynamic::*;
pub use list::*;
pub use type_check::*;
