#![no_std]

#[cfg(feature = "std")]
extern crate std;

mod fixed_buffer;
#[cfg(feature = "std")]
mod stdio;

use core::fmt::Debug;
pub use fixed_buffer::FixedBufferDevice;
#[cfg(feature = "std")]
pub use stdio::StdioDevice;

pub trait Device {
    type Error: Debug;

    fn read(&mut self) -> Result<u8, Self::Error>;
    fn write(&mut self, byte: u8) -> Result<(), Self::Error>;
}
