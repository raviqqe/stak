//! Primitive sets.
//!
//! This crate provides [`PrimitiveSet`](vm::PrimitiveSet)s necessary to run basic programs in Scheme.

#![no_std]

#[cfg(feature = "std")]
extern crate std;

mod small;

pub use small::SmallPrimitiveSet;
