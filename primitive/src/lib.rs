#![no_std]

#[cfg(any(feature = "std"))]
extern crate std;

mod small;

pub use small::SmallPrimitiveSet;
