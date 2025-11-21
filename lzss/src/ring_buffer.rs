#[derive(Debug)]
pub struct RingBuffer<const N: usize> {
    buffer: [u8; N],
    index: usize,
}

impl<const N: usize> RingBuffer<N> {
    pub const fn new() -> Self {
        Self {
            buffer: [0; N],
            index: 0,
        }
    }

    pub fn get(&self, index: usize) -> Option<u8> {
        self.buffer.get(self.index(index)).copied()
    }

    pub const fn push(&mut self, byte: u8) {
        self.buffer[self.index] = byte;
        self.index = self.index(1);
    }

    const fn index(&self, index: usize) -> usize {
        (self.index + index) % N
    }
}

impl<const N: usize> Default for RingBuffer<N> {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn push() {
        let mut buffer = RingBuffer::<3>::new();

        buffer.push(42);

        assert_eq!(buffer.get(0), Some(0));
        assert_eq!(buffer.get(2), Some(42));
    }

    #[test]
    fn push_over_end() {
        let mut buffer = RingBuffer::<3>::new();

        buffer.push(1);
        buffer.push(2);
        buffer.push(3);

        assert_eq!(buffer.get(0), Some(1));
        assert_eq!(buffer.get(1), Some(2));
        assert_eq!(buffer.get(2), Some(3));
        assert_eq!(buffer.get(3), Some(1));

        buffer.push(4);

        assert_eq!(buffer.get(0), Some(2));
        assert_eq!(buffer.get(1), Some(3));
        assert_eq!(buffer.get(2), Some(4));
        assert_eq!(buffer.get(3), Some(2));
    }
}
