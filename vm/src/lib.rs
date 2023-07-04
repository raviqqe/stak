#![no_std]

#[cfg(test)]
extern crate std;

mod cons;
mod device;
mod error;
mod instruction;
mod number;
mod primitive;
mod value;
mod vm;

pub use device::{Device, FixedBufferDevice};
pub use error::Error;
pub use vm::Vm;
