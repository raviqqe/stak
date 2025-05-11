mod buffer_error;
mod fixed_buffer;
#[cfg(feature = "libc")]
pub mod libc;
#[cfg(feature = "std")]
mod read_write;
#[cfg(feature = "std")]
mod stdio;
mod void;

pub use buffer_error::BufferError;
use core::error::Error;
pub use fixed_buffer::FixedBufferDevice;
#[cfg(feature = "std")]
pub use read_write::ReadWriteDevice;
#[cfg(feature = "std")]
pub use stdio::StdioDevice;
pub use void::*;
use winter_maybe_async::{maybe_async, maybe_await};

/// A device.
pub trait Device {
    /// An error.
    type Error: Error;

    /// Reads from standard input.
    #[maybe_async]
    fn read(&mut self) -> Result<Option<u8>, Self::Error>;

    /// Writes to standard output.
    #[maybe_async]
    fn write(&mut self, byte: u8) -> Result<(), Self::Error>;

    /// Writes to standard error.
    #[maybe_async]
    fn write_error(&mut self, byte: u8) -> Result<(), Self::Error>;
}

impl<T: Device> Device for &mut T {
    type Error = T::Error;

    #[maybe_async]
    fn read(&mut self) -> Result<Option<u8>, Self::Error> {
        maybe_await!((**self).read())
    }

    #[maybe_async]
    fn write(&mut self, byte: u8) -> Result<(), Self::Error> {
        maybe_await!((**self).write(byte))
    }

    #[maybe_async]
    fn write_error(&mut self, byte: u8) -> Result<(), Self::Error> {
        maybe_await!((**self).write_error(byte))
    }
}
