use super::{Read, Write};
use core::fmt::{self, Display};

#[derive(Debug)]
pub struct Buffer<'a> {
    data: &'a [u8],
    index: usize,
}

impl<'a> Buffer<'a> {
    pub fn new(data: &'a [u8]) -> Self {
        Self { data, index: 0 }
    }
}

impl Read for Buffer<'_> {
    type Error = BufferError;

    fn read(&mut self) -> Result<Option<u8>, Self::Error> {
        Ok(if let Some(&byte) = self.data.get(self.index) {
            self.index += 1;

            Some(byte)
        } else {
            None
        })
    }
}

#[derive(Debug)]
pub struct BufferMut<'a> {
    data: &'a mut [u8],
    index: usize,
}

impl<'a> BufferMut<'a> {
    pub fn new(data: &'a mut [u8]) -> Self {
        Self { data, index: 0 }
    }

    pub fn as_bytes(&self) -> &[u8] {
        &self.data[..self.index]
    }
}

impl Write for BufferMut<'_> {
    type Error = BufferError;

    fn write(&mut self, byte: u8) -> Result<(), BufferError> {
        *self.data.get_mut(self.index).ok_or(BufferError::Write)? = byte;

        self.index += 1;

        Ok(())
    }
}

#[derive(Clone, Debug)]
pub enum BufferError {
    Read,
    Write,
}

impl Display for BufferError {
    fn fmt(&self, _: &mut Formatter) -> fmt::Result {
        match self {
            Self::Read => write!(f, "failed to read buffer"),
            Self::Write => write!(f, "failed to write buffer"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn read() {
        let mut buffer = Buffer::new(&[1, 2, 3]);

        assert_eq!(buffer.read().unwrap(), Some(1));
        assert_eq!(buffer.read().unwrap(), Some(2));
        assert_eq!(buffer.read().unwrap(), Some(3));
        assert_eq!(buffer.read().unwrap(), None);
        assert_eq!(buffer.read().unwrap(), None);
    }

    #[test]
    fn write() {
        let mut array = [0; 3];
        let mut buffer = BufferMut::new(&mut array);

        assert_eq!(buffer.write(1), Ok(()));
        assert_eq!(buffer.write(2), Ok(()));
        assert_eq!(buffer.write(3), Ok(()));
        assert_eq!(buffer.write(4), Err(()));
        assert_eq!(buffer.write(5), Err(()));
        assert_eq!(array, [1, 2, 3]);
    }
}
