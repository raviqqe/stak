#[derive(Debug)]
pub struct RingBuffer<const N: usize> {
    buffer: [u8; N],
    end: usize,
}

impl<const N: usize> RingBuffer<N> {
    pub fn new() -> Self {
        Self {
            buffer: [0; N],
            end: 0,
        }
    }

    pub fn get(&self, index: usize) -> Option<u8> {
        self.buffer.get(self.index(index)).copied()
    }

    pub fn push(&mut self, byte: u8) {
        self.end = (self.end + 1) % N;
        self.buffer[self.end] = byte;
    }

    fn index(&self, index: usize) -> usize {
        (N + self.end - index) % N
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
        assert_eq!(buffer.get(1), Some(2));
        assert_eq!(buffer.get(2), Some(1));
        assert_eq!(buffer.get(3), Some(3));

        buffer.push(4);

        assert_eq!(buffer.get(0), Some(4));
        assert_eq!(buffer.get(1), Some(3));
        assert_eq!(buffer.get(2), Some(2));
        assert_eq!(buffer.get(3), Some(4));
    }
}
