//! Devices to handle I/O.

#![no_std]

#[cfg(feature = "std")]
extern crate std;

mod device;
mod fixed_buffer;
mod read_write;
#[cfg(feature = "std")]
mod stdio;

pub use device::Device;
pub use fixed_buffer::FixedBufferDevice;
pub use read_write::ReadWriteDevice;
#[cfg(feature = "std")]
pub use stdio::StdioDevice;
