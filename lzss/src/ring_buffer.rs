use core::ops::Index;

#[derive(Debug)]
pub struct RingBuffer<const N: usize> {
    buffer: [u8; N],
    offset: usize,
}

impl<const N: usize> RingBuffer<N> {
    pub const fn new() -> Self {
        Self {
            buffer: [0; N],
            offset: 0,
        }
    }

    pub const fn push(&mut self, byte: u8) {
        self.buffer[self.offset] = byte;
        self.offset = self.index(1);
    }

    const fn index(&self, index: usize) -> usize {
        (self.offset + index) % N
    }
}

impl<const N: usize> Index<usize> for RingBuffer<N> {
    type Output = u8;

    fn index(&self, index: usize) -> &Self::Output {
        &self.buffer[self.index(index)]
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

        assert_eq!(buffer[0], 0);
        assert_eq!(buffer[2], 42);
    }

    #[test]
    fn push_over_end() {
        let mut buffer = RingBuffer::<3>::new();

        buffer.push(1);
        buffer.push(2);
        buffer.push(3);

        assert_eq!(buffer[0], 1);
        assert_eq!(buffer[1], 2);
        assert_eq!(buffer[2], 3);
        assert_eq!(buffer[3], 1);

        buffer.push(4);

        assert_eq!(buffer[0], 2);
        assert_eq!(buffer[1], 3);
        assert_eq!(buffer[2], 4);
        assert_eq!(buffer[3], 2);
    }
}
