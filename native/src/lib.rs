//! Stak Scheme primitive sets for optimized primitives of native functions.

#![no_std]

#[cfg(feature = "alloc")]
extern crate alloc;

#[cfg(feature = "alloc")]
pub mod dynamic;
mod list;
mod type_check;

pub use list::*;
pub use type_check::*;
