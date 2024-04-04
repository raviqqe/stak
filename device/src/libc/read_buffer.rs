use super::Read;
use core::fmt::{self, Display};

#[derive(Debug)]
pub struct ReadBuffer<'a> {
    data: &'a [u8],
    index: usize,
}

impl<'a> ReadBuffer<'a> {
    pub fn new(data: &'a [u8]) -> Self {
        Self { data, index: 0 }
    }
}

impl Read for ReadBuffer<'_> {
    type Error = ReadBufferError;

    fn read(&mut self) -> Result<Option<u8>, Self::Error> {
        Ok(if let Some(&byte) = self.data.get(self.index) {
            self.index += 1;

            Some(byte)
        } else {
            None
        })
    }
}

#[derive(Clone, Debug)]
pub enum ReadBufferError {
    Read,
}

impl Display for ReadBufferError {
    fn fmt(&self, _: &mut Formatter) -> fmt::Result {
        match self {
            ReadBufferError::Read => write!(f, "failed to read buffer"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn read() {
        let mut buffer = ReadBuffer::new(&[1, 2, 3]);

        assert_eq!(buffer.read().unwrap(), Some(1));
        assert_eq!(buffer.read().unwrap(), Some(2));
        assert_eq!(buffer.read().unwrap(), Some(3));
        assert_eq!(buffer.read().unwrap(), None);
        assert_eq!(buffer.read().unwrap(), None);
    }
}
