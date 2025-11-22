use crate::ring_buffer::RingBuffer;

const MIN_LENGTH: usize = 2;
/// The maximum match length.
pub const MAX_LENGTH: usize = u8::MAX as _;
/// The default window size.
pub const DEFAULT_WINDOW_SIZE: usize = (1 << 7) - 1;

/// LZSS decompression iterator.
pub struct LzssDecompressionIterator<const W: usize, I: Iterator<Item = u8>> {
    iterator: I,
    buffer: RingBuffer<W>,
    offset: u8,
    length: u8,
}

impl<const W: usize, I: Iterator<Item = u8>> LzssDecompressionIterator<W, I> {
    fn new(iterator: I) -> Self {
        Self {
            iterator,
            buffer: Default::default(),
            offset: 0,
            length: 0,
        }
    }
}

impl<const W: usize, I: Iterator<Item = u8>> Iterator for LzssDecompressionIterator<W, I> {
    type Item = u8;

    fn next(&mut self) -> Option<Self::Item> {
        if self.length > 0 {
            let x = self.buffer[W - 1 - self.offset as usize];

            self.buffer.push(x);
            self.length -= 1;

            Some(x)
        } else {
            let x = self.iterator.next()?;
            let y = x >> 1;

            if x.is_multiple_of(2) {
                self.buffer.push(y);
                Some(y)
            } else {
                self.offset = y;
                self.length = self.iterator.next()?;

                self.next()
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use alloc::vec::Vec;
    use core::iter::repeat;
    use pretty_assertions::assert_eq;
    use quickcheck_macros::quickcheck;

    #[test]
    fn repetition() {
        assert_eq!(
            [2, 4, 6, 8, 7, 5].decompress::<8>().collect::<Vec<_>>(),
            [1, 2, 3, 4, 1, 2, 3, 4, 1]
        );
    }

    #[test]
    fn repetitions() {
        assert_eq!(
            [2, 4, 6, 8, 7, 5, 10, 12, 3, 3]
                .decompress::<8>()
                .collect::<Vec<_>>(),
            [1, 2, 3, 4, 1, 2, 3, 4, 1, 5, 6, 5, 6, 5]
        );
    }

    #[test]
    fn max_length() {
        assert_eq!(
            [84, 1, 255].decompress::<1>().collect::<Vec<_>>(),
            repeat(42).take(256).collect::<Vec<_>>()
        );
    }

    #[test]
    fn max_offset() {
        assert_eq!(
            (0..128)
                .map(|x| x << 1)
                .chain([255, 128])
                .decompress::<128>()
                .collect::<Vec<_>>(),
            (0..128).chain(0..128).collect::<Vec<_>>()
        );
    }
}
