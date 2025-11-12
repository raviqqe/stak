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
