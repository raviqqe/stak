//! File systems.

#![cfg_attr(doc, feature(doc_cfg))]
#![no_std]

#[cfg(test)]
extern crate alloc;
#[cfg(any(feature = "std", test))]
extern crate std;

mod file_system;
mod primitive_set;

pub use file_system::*;
pub use primitive_set::{FilePrimitiveSet, Primitive};
