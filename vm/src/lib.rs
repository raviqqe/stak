#![no_std]

#[cfg(test)]
extern crate alloc;
#[cfg(test)]
extern crate std;

mod cons;
mod device;
mod error;
mod instruction;
mod number;
mod primitive;
#[cfg(test)]
mod symbol_index;
mod r#type;
mod value;
mod vm;

pub use device::{Device, FixedBufferDevice};
pub use error::Error;
pub use value::Value;
pub use vm::Vm;
