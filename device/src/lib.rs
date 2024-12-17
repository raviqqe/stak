//! Devices to handle I/O.

#![cfg_attr(all(doc, not(clippy), not(doctest)), feature(doc_cfg))]
#![no_std]

#[cfg(test)]
extern crate alloc;
#[cfg(feature = "std")]
extern crate std;

mod device;
mod primitive_set;

pub use device::*;
pub use primitive_set::{DevicePrimitiveSet, Primitive, PrimitiveError};
