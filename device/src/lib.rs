//! Devices to handle I/O.

#![no_std]

#[cfg(test)]
extern crate alloc;
#[cfg(feature = "std")]
extern crate std;

mod buffer_error;
mod device;
mod fixed_buffer;
#[cfg(feature = "libc")]
pub mod libc;
mod primitive_set;
#[cfg(feature = "std")]
mod read_write;
#[cfg(feature = "std")]
mod stdio;

pub use buffer_error::BufferError;
pub use device::Device;
pub use fixed_buffer::FixedBufferDevice;
pub use primitive_set::{DevicePrimitiveSet, Primitive, PrimitiveError};
#[cfg(feature = "std")]
pub use read_write::ReadWriteDevice;
#[cfg(feature = "std")]
pub use stdio::StdioDevice;
