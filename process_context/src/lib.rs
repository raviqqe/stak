//! A process context for Stak Scheme.

#![cfg_attr(all(doc, not(doctest)), feature(doc_auto_cfg))]
#![no_std]

#[cfg(feature = "std")]
extern crate alloc;

#[cfg(feature = "std")]
extern crate std;

mod primitive_set;
mod process_context;

pub use primitive_set::{Primitive, ProcessContextPrimitiveSet};
pub use process_context::*;
