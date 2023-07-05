#![no_std]

extern crate alloc;
#[cfg(test)]
extern crate std;

mod error;
mod ir;

pub use error::Error;
pub use ir::*;
