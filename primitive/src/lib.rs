#![no_std]

#[cfg(feature = "std")]
extern crate std;

mod small;

pub use small::SmallPrimitiveSet;
