use core::error::Error;

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
