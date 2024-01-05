//! Devices to handle I/O.

#![no_std]

#[cfg(feature = "std")]
extern crate std;

mod custom;
mod device;
mod fixed_buffer;
#[cfg(feature = "std")]
mod stdio;

pub use custom::CustomDevice;
pub use device::Device;
pub use fixed_buffer::FixedBufferDevice;
#[cfg(feature = "std")]
pub use stdio::StdioDevice;
