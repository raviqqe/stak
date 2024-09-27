//! Time for Stak Scheme.

#![no_std]

#[cfg(feature = "std")]
extern crate alloc;

#[cfg(feature = "std")]
extern crate std;

mod clock;
mod primitive;

pub use clock::*;
pub use primitive::{Primitive, TimePrimitiveSet};
