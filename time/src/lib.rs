//! Time for Stak Scheme.

#![no_std]

#[cfg(feature = "std")]
extern crate std;

mod clock;
mod primitive_set;

pub use clock::*;
pub use primitive_set::{Primitive, TimePrimitiveSet};
