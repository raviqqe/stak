use rustix::{fd::BorrowedFd, io};

pub trait Source {
    fn read(&mut self, buffer: &mut [u8]) -> io::Result<usize>;
}

impl Source for BorrowedFd<'_> {
    fn read(&mut self, buffer: &mut [u8]) -> io::Result<usize> {
        io::read(*self, buffer)
    }
}
