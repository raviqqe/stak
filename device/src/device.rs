mod buffer_error;
mod fixed_buffer;
#[doc(cfg(feature = "libc"))]
#[cfg(feature = "libc")]
pub mod libc;
#[doc(cfg(feature = "std"))]
#[cfg(feature = "std")]
mod read_write;
#[doc(cfg(feature = "std"))]
#[cfg(feature = "std")]
mod stdio;

pub use buffer_error::BufferError;
use core::error::Error;
pub use fixed_buffer::FixedBufferDevice;
#[cfg(feature = "std")]
pub use read_write::ReadWriteDevice;
#[cfg(feature = "std")]
pub use stdio::StdioDevice;

/// A device.
pub trait Device {
    /// An error.
    type Error: Error;

    /// Reads from standard input.
    fn read(&mut self) -> Result<Option<u8>, Self::Error>;
    /// Writes to standard output.
    fn write(&mut self, byte: u8) -> Result<(), Self::Error>;
    /// Writes to standard error.
    fn write_error(&mut self, byte: u8) -> Result<(), Self::Error>;
}
