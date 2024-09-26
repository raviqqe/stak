//! Stak Scheme primitive sets for R7RS.
//!
//! This crate provides [`PrimitiveSet`](stak_vm::PrimitiveSet)s that covers R7RS.

#![no_std]

mod small;

pub use small::{Error as SmallError, SmallPrimitiveSet};
