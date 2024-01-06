//! Primitive sets.
//!
//! This crate provides [`PrimitiveSet`](vm::PrimitiveSet)s to run basic
//! programs in Scheme.

#![no_std]

#[cfg(feature = "std")]
extern crate std;

mod small;

pub use small::{Error as SmallError, SmallPrimitiveSet};
