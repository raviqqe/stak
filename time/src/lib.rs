//! Time for Stak Scheme.

#![cfg_attr(all(doc, not(doctest)), feature(doc_auto_cfg))]
#![no_std]

#[cfg(feature = "std")]
extern crate std;

mod clock;
mod primitive_set;

pub use clock::*;
pub use primitive_set::{Primitive, TimePrimitiveSet};
