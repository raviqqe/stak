use crate::{BufferError, Device};
use winter_maybe_async::maybe_async;

/// A void device where all I/O operations succeed with no side effect.
#[derive(Debug, Default)]
pub struct VoidDevice {}

impl VoidDevice {
    /// Creates a device.
    pub fn new() -> Self {
        Self::default()
    }
}

impl Device for VoidDevice {
    type Error = BufferError;

    #[maybe_async]
    fn read(&mut self) -> Result<Option<u8>, Self::Error> {
        Ok(None)
    }

    #[maybe_async]
    fn write(&mut self, _byte: u8) -> Result<(), Self::Error> {
        Ok(())
    }

    #[maybe_async]
    fn write_error(&mut self, _byte: u8) -> Result<(), Self::Error> {
        Ok(())
    }
}
