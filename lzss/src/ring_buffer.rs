#[derive(Debug)]
pub struct RingBuffer<const N: usize> {
    buffer: [u8; N],
    index: usize,
}

impl<const N: usize> RingBuffer<N> {
    pub fn new() -> Self {
        Self {
            buffer: [0; N],
            index: 0,
        }
    }

    pub const fn len(&self) -> usize{
        N
    }

    pub fn get(&self, index: usize) -> Option<u8> {
        self.buffer.get(self.index(index)).copied()
    }

    pub fn push(&mut self, byte: u8) {
        self.index = (self.index + 1) % N;
        self.buffer[self.index] = byte;
    }

    fn index(&self, index: usize) -> usize {
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

    #[test]
    fn index() {
        let mut buffer = RingBuffer::<3>::new();

        buffer.push(1);
        buffer.push(2);
        buffer.push(3);

        assert_eq!(buffer.get(0), Some(3));
        assert_eq!(buffer.get(1), Some(1));
        assert_eq!(buffer.get(2), Some(2));
        assert_eq!(buffer.get(3), Some(3));

        buffer.push(4);

        assert_eq!(buffer.get(0), Some(4));
        assert_eq!(buffer.get(1), Some(2));
        assert_eq!(buffer.get(2), Some(3));
        assert_eq!(buffer.get(3), Some(4));
    }
}
