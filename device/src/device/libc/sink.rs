use rustix::{fd::BorrowedFd, io};

pub trait Sink {
    fn write(&mut self, buffer: &[u8]) -> io::Result<usize>;
}

impl Sink for BorrowedFd<'_> {
    fn write(&mut self, buffer: &[u8]) -> io::Result<usize> {
        io::write(*self, buffer)
    }
}
