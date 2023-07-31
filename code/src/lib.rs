#![no_std]

#[cfg(feature = "alloc")]
extern crate alloc;
#[cfg(test)]
extern crate std;

#[cfg(feature = "alloc")]
mod decode;
#[cfg(feature = "alloc")]
mod encode;
#[cfg(feature = "alloc")]
mod error;
#[cfg(feature = "alloc")]
mod ir;

#[cfg(feature = "alloc")]
pub use decode::decode;
#[cfg(feature = "alloc")]
pub use encode::encode;
#[cfg(feature = "alloc")]
pub use error::Error;
#[cfg(feature = "alloc")]
pub use ir::*;

pub const INTEGER_BASE: u64 = i8::MAX as u64 + 1;
pub const SHORT_INTEGER_BITS: u64 = 3;
pub const INSTRUCTION_BITS: u64 = 8 - SHORT_INTEGER_BITS - 1;
pub const SHORT_INTEGER_BASE: u64 = 1 << SHORT_INTEGER_BITS;
