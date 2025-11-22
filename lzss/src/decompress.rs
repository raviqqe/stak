use crate::ring_buffer::RingBuffer;

/// LZSS decompression iterator.
pub struct LzssDecompressionIterator<const W: usize, I: Iterator<Item = u8>> {
    iterator: I,
    buffer: RingBuffer<W>,
    offset: u8,
    length: u8,
}

impl<const W: usize, I: Iterator<Item = u8>> LzssDecompressionIterator<W, I> {
    pub fn new(iterator: I) -> Self {
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

    #[test]
    fn repetition() {
        assert_eq!(
            LzssDecompressionIterator::<8, _>::new([2, 4, 6, 8, 7, 5].into_iter())
                .collect::<Vec<_>>(),
            [1, 2, 3, 4, 1, 2, 3, 4, 1]
        );
    }

    #[test]
    fn repetitions() {
        assert_eq!(
            LzssDecompressionIterator::<8, _>::new([2, 4, 6, 8, 7, 5, 10, 12, 3, 3].into_iter())
                .collect::<Vec<_>>(),
            [1, 2, 3, 4, 1, 2, 3, 4, 1, 5, 6, 5, 6, 5]
        );
    }

    #[test]
    fn max_length() {
        assert_eq!(
            LzssDecompressionIterator::<1, _>::new([84, 1, 255].into_iter()).collect::<Vec<_>>(),
            repeat(42).take(256).collect::<Vec<_>>()
        );
    }

    #[test]
    fn max_offset() {
        assert_eq!(
            LzssDecompressionIterator::<128, _>::new((0..128).map(|x| x << 1).chain([255, 128]))
                .collect::<Vec<_>>(),
            (0..128).chain(0..128).collect::<Vec<_>>()
        );
    }
}
