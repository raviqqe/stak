use rustix::{fd::BorrowedFd, io};

/// A source of bytes.
pub trait Source {
    /// Reads bytes into a buffer and returns the number of read bytes.
    fn read(&mut self, buffer: &mut [u8]) -> io::Result<usize>;
}

impl Source for BorrowedFd<'_> {
    fn read(&mut self, buffer: &mut [u8]) -> io::Result<usize> {
        io::read(*self, buffer)
    }
}
