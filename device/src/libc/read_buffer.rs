#[derive(Debug)]
pub struct ReadBuffer<'a> {
    data: &'a [u8],
    index: usize,
}

impl<'a> ReadBuffer<'a> {
    pub fn new(data: &'a [u8]) -> Self {
        Self { data, index: 0 }
    }

    pub fn read(&mut self) -> Option<u8> {
        if let Some(&byte) = self.data.get(self.index) {
            self.index += 1;

            Some(byte)
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn read() {
        let mut buffer = ReadBuffer::new(&[1, 2, 3]);

        assert_eq!(buffer.read(), Some(1));
        assert_eq!(buffer.read(), Some(2));
        assert_eq!(buffer.read(), Some(3));
        assert_eq!(buffer.read(), None);
        assert_eq!(buffer.read(), None);
    }
}
