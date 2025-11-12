#[derive(Default)]
pub struct RingBuffer<const W: usize> {
    buffer: [u8; W],
    index: usize,
}

impl<const W: usize> RingBuffer<W> {
    /// Creates a buffer.
    pub fn new() -> Self {
        Self {
            buffer: [0; W],
            index: 0,
        }
    }

    /// Returns an item with a backward index.
    pub fn get(&self, index: usize) -> Option<u8> {
        self.buffer.get(self.index(index)).copied()
    }

    /// Appends an item.
    pub fn append(&mut self, byte: u8) {
        self.index = (self.index + 1) % W;
        self.buffer[self.index] = byte;
    }

    fn index(&self, index: usize) -> usize {
        (self.index - index) % W
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn index() {
        let mut buffer: RingBuffer<4> = RingBuffer::new();

        buffer.append(1);
        buffer.append(2);
        buffer.append(3);
        buffer.append(4);

        assert_eq!(buffer.get(0), Some(4));
        assert_eq!(buffer.get(1), Some(3));
        assert_eq!(buffer.get(2), Some(2));
        assert_eq!(buffer.get(3), Some(1));
        assert_eq!(buffer.get(4), Some(4));

        buffer.append(5);

        assert_eq!(buffer.get(0), Some(5));
        assert_eq!(buffer.get(1), Some(4));
        assert_eq!(buffer.get(2), Some(3));
        assert_eq!(buffer.get(3), Some(2));
        assert_eq!(buffer.get(4), Some(5));
    }
}
