#[derive(Debug)]
pub struct WriteBuffer<'a> {
    data: &'a mut [u8],
    index: usize,
}

impl<'a> WriteBuffer<'a> {
    pub fn new(data: &'a mut [u8]) -> Self {
        Self { data, index: 0 }
    }

    pub fn write(&mut self, byte: u8) -> bool {
        if let Some(pointer) = self.data.get_mut(self.index) {
            *pointer = byte;

            self.index += 1;

            false
        } else {
            true
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

        assert_eq!(buffer.write(1), false);
        assert_eq!(buffer.write(2), false);
        assert_eq!(buffer.write(3), false);
        assert_eq!(buffer.write(4), true);
        assert_eq!(buffer.write(5), true);
        assert_eq!(array, [1, 2, 3]);
    }
}
