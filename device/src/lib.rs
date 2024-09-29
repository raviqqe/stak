//! Devices to handle I/O.

#![no_std]

#[cfg(test)]
extern crate alloc;
#[cfg(feature = "std")]
extern crate std;

mod device;
mod primitive_set;

pub use device::*;
pub use primitive_set::{DevicePrimitiveSet, Primitive, PrimitiveError};
