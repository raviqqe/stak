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
                self.length = y;
                self.offset = self.iterator.next()?;

                self.next()
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{MAX_LENGTH, MAX_WINDOW_SIZE};
    use alloc::vec::Vec;
    use core::iter::repeat;
    use pretty_assertions::assert_eq;

    #[test]
    fn repetition() {
        assert_eq!(
            LzssDecompressionIterator::<8, _>::new([2, 4, 6, 8, 11, 3].into_iter())
                .collect::<Vec<_>>(),
            [1, 2, 3, 4, 1, 2, 3, 4, 1]
        );
    }

    #[test]
    fn repetitions() {
        assert_eq!(
            LzssDecompressionIterator::<8, _>::new([2, 4, 6, 8, 11, 3, 10, 12, 7, 1].into_iter())
                .collect::<Vec<_>>(),
            [1, 2, 3, 4, 1, 2, 3, 4, 1, 5, 6, 5, 6, 5]
        );
    }

    #[test]
    fn max_length() {
        assert_eq!(
            LzssDecompressionIterator::<1, _>::new([84, (MAX_LENGTH as u8) << 1 | 1, 0].into_iter())
                .collect::<Vec<_>>(),
            repeat(42).take(MAX_LENGTH + 1).collect::<Vec<_>>()
        );
    }

    #[test]
    fn max_offset() {
        let offset = MAX_WINDOW_SIZE as u8;

        assert_eq!(
            LzssDecompressionIterator::<MAX_WINDOW_SIZE, _>::new(
                (0..offset).map(|x| x << 1).chain([255, offset])
            )
            .collect::<Vec<_>>(),
            (0..offset).chain(0..offset).collect::<Vec<_>>()
        );
    }
}
