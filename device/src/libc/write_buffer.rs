use super::Write;

#[derive(Debug)]
pub struct WriteBuffer<'a> {
    data: &'a mut [u8],
    index: usize,
}

impl<'a> WriteBuffer<'a> {
    pub fn new(data: &'a mut [u8]) -> Self {
        Self { data, index: 0 }
    }
}

impl Write for WriteBuffer<'_> {
    type Error = ();

    fn write(&mut self, byte: u8) -> Result<(), ()> {
        if let Some(pointer) = self.data.get_mut(self.index) {
            *pointer = byte;

            self.index += 1;

            Ok(())
        } else {
            Err(())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_write_buffer() {
        let mut array = [0; 3];
        let mut buffer = WriteBuffer::new(&mut array);

        assert_eq!(buffer.write(1), Ok(()));
        assert_eq!(buffer.write(2), Ok(()));
        assert_eq!(buffer.write(3), Ok(()));
        assert_eq!(buffer.write(4), Err(()));
        assert_eq!(buffer.write(5), Err(()));
        assert_eq!(array, [1, 2, 3]);
    }
}
