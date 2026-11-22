use rustix::{fd::BorrowedFd, io};

/// A sink of bytes.
pub trait Sink {
    /// Writes bytes in a buffer and returns the number of written bytes.
    fn write(&mut self, buffer: &[u8]) -> io::Result<usize>;
}

impl Sink for BorrowedFd<'_> {
    fn write(&mut self, buffer: &[u8]) -> io::Result<usize> {
        io::write(*self, buffer)
    }
}
