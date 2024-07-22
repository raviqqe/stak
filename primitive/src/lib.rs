//! Primitive sets.
//!
//! This crate provides [`PrimitiveSet`](stak_vm::PrimitiveSet)s to run basic
//! programs in Scheme.

#![no_std]

#[cfg(feature = "std")]
extern crate std;

mod composite;
mod small;

pub use composite::CompositePrimitiveSet;
pub use small::{Error as SmallError, SmallPrimitiveSet};
