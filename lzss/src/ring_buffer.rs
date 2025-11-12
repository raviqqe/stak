pub struct RingBuffer {
    buffer: [u8; W],
    index: usize,
}
