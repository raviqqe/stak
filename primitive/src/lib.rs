//! Primitive sets.
//!
//! We equip [`vm::Vm`]s with [`vm::PrimitiveSet`]s that provide small functionalities,
//! such as arithmetic and I/O.
//!
//! This crate provides primitive sets necessary to run basic programs in Scheme.

#![no_std]

#[cfg(feature = "std")]
extern crate std;

mod small;

pub use small::SmallPrimitiveSet;
