use core::fmt::Display;

/// A trait that writes a byte.
pub trait Write {
    /// An error.
    type Error: Display;

    /// Writes a byte.
    fn write(&mut self, byte: u8) -> Result<(), Self::Error>;
}

impl<T: Write> Write for &mut T {
    type Error = T::Error;

    fn write(&mut self, byte: u8) -> Result<(), Self::Error> {
        (**self).write(byte)
    }
}
