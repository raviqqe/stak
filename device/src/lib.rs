//! Devices to handle I/O.

#![no_std]

#[cfg(test)]
extern crate alloc;
#[cfg(feature = "std")]
extern crate std;

mod device;
mod fixed_buffer;
#[cfg(feature = "libc")]
pub mod libc;
pub mod primitive_set;
#[cfg(feature = "std")]
mod read_write;
#[cfg(feature = "std")]
mod stdio;

pub use device::Device;
pub use fixed_buffer::FixedBufferDevice;
#[cfg(feature = "std")]
pub use read_write::ReadWriteDevice;
#[cfg(feature = "std")]
pub use stdio::StdioDevice;
