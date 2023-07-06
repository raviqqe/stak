#![no_std]

extern crate alloc;
#[cfg(test)]
extern crate std;

mod decode;
mod encode;
mod error;
mod ir;

pub use decode::decode;
pub use encode::{encode, INTEGER_BASE};
pub use error::Error;
pub use ir::*;
