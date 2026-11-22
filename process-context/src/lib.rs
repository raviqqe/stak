//! A process context for Stak Scheme.

#![cfg_attr(all(doc, not(doctest)), feature(doc_cfg))]
#![no_std]

#[cfg(any(feature = "std", test))]
extern crate alloc;

#[cfg(any(feature = "std", test))]
extern crate std;

mod primitive_set;
mod process_context;

pub use primitive_set::{Primitive, ProcessContextPrimitiveSet};
pub use process_context::*;
