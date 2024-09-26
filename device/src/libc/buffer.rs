use super::{Read, Write};
use crate::BufferError;

/// An immutable buffer.
#[derive(Debug)]
pub struct Buffer<'a> {
    data: &'a [&'a [u8]],
    buffer_index: usize,
    byte_index: usize,
}

impl<'a> Buffer<'a> {
    /// Creates an immutable buffer.
    pub const fn new(data: &'a [&'a [u8]]) -> Self {
        Self {
            data,
            buffer_index: 0,
            byte_index: 0,
        }
    }

    fn read_raw(&mut self) -> Option<u8> {
        let data = self.data.get(self.buffer_index)?;
        let Some(&byte) = data.get(self.byte_index) else {
            self.buffer_index += 1;
            self.byte_index = 0;

            return self.read_raw();
        };

        self.byte_index += 1;

        Some(byte)
    }
}

impl Read for Buffer<'_> {
    type Error = BufferError;

    fn read(&mut self) -> Result<Option<u8>, Self::Error> {
        Ok(self.read_raw())
    }
}

/// A mutable buffer.
#[derive(Debug)]
pub struct BufferMut<'a> {
    data: &'a mut [u8],
    index: usize,
}

impl<'a> BufferMut<'a> {
    /// Creates a mutable buffer.
    pub fn new(data: &'a mut [u8]) -> Self {
        Self { data, index: 0 }
    }

    /// Returns bytes.
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn read() {
        let mut buffer = Buffer::new(&[&[1, 2, 3]]);

        assert_eq!(buffer.read().unwrap(), Some(1));
        assert_eq!(buffer.read().unwrap(), Some(2));
        assert_eq!(buffer.read().unwrap(), Some(3));
        assert_eq!(buffer.read().unwrap(), None);
        assert_eq!(buffer.read().unwrap(), None);
    }

    #[test]
    fn read_many() {
        let mut buffer = Buffer::new(&[&[], &[1], &[2, 3], &[], &[4], &[5, 6]]);

        assert_eq!(buffer.read().unwrap(), Some(1));
        assert_eq!(buffer.read().unwrap(), Some(2));
        assert_eq!(buffer.read().unwrap(), Some(3));
        assert_eq!(buffer.read().unwrap(), Some(4));
        assert_eq!(buffer.read().unwrap(), Some(5));
        assert_eq!(buffer.read().unwrap(), Some(6));
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
        assert_eq!(buffer.write(4), Err(BufferError::Write));
        assert_eq!(buffer.write(5), Err(BufferError::Write));
        assert_eq!(array, [1, 2, 3]);
    }
}
