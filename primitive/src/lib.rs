//! Primitive sets.
//!
//! We equip virtual machines of [`vm::Vm`] with primitive sets that provide small primitive functionalities,
//! such as I/O, arithmetic, and so on.
//!
//! This crate provides primitive sets necessary to run basic programs in Scheme.

#![no_std]

#[cfg(feature = "std")]
extern crate std;

mod small;

pub use small::SmallPrimitiveSet;
