use core::fmt::Display;

/// A trait that reads a byte.
pub trait Read {
    /// An error.
    type Error: Display;

    /// Reads a byte.
    fn read(&mut self) -> Result<Option<u8>, Self::Error>;
}

impl<T: Read> Read for &mut T {
    type Error = T::Error;

    fn read(&mut self) -> Result<Option<u8>, Self::Error> {
        (**self).read()
    }
}
