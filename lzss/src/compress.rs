use crate::ring_buffer::RingBuffer;

const MIN_LENGTH: usize = 2;
/// The maximum match length.
pub const MAX_LENGTH: usize = u8::MAX as _;

/// LZSS compression iterator.
pub struct LzssCompressionIterator<const B: usize, I: Iterator<Item = u8>> {
    iterator: I,
    buffer: RingBuffer<B>,
    ahead: usize,
    next: Option<u8>,
}

impl<const B: usize, I: Iterator<Item = u8>> LzssCompressionIterator<B, I> {
    const WINDOW_SIZE: usize = B - MAX_LENGTH;

    pub fn new(iterator: I) -> Self {
        Self {
            iterator,
            buffer: Default::default(),
            ahead: Default::default(),
            next: Default::default(),
        }
    }

    fn next(&mut self) -> Option<u8> {
        if self.ahead > 0 {
            let x = self.buffer[B - self.ahead];
            self.ahead -= 1;
            Some(x)
        } else if let Some(x) = self.iterator.next() {
            self.buffer.push(x);
            Some(x)
        } else {
            None
        }
    }

    fn peek(&mut self, index: usize) -> Option<u8> {
        Some(if index < self.ahead {
            self.buffer[B - self.ahead + index]
        } else {
            let mut x = 0;

            for _ in 0..index + 1 - self.ahead {
                x = self.iterator.next()?;
                self.buffer.push(x);
                self.ahead += 1;
            }

            x
        })
    }
}

impl<const B: usize, I: Iterator<Item = u8>> Iterator for LzssCompressionIterator<B, I> {
    type Item = u8;

    fn next(&mut self) -> Option<Self::Item> {
        Some(if let Some(x) = self.next {
            self.next = None;
            x
        } else {
            // This implementation reads uninitialized zeros from the buffer.
            let (n, m) = (0..Self::WINDOW_SIZE)
                .map(|i| {
                    let mut j = 0;

                    while j < MAX_LENGTH
                        && self.peek(j) == Some(self.buffer[2 * B - self.ahead - 1 - i + j])
                    {
                        j += 1;
                    }

                    (i, j)
                })
                .max_by_key(|(_, j)| *j)
                .unwrap_or_default();

            if m > MIN_LENGTH {
                self.next = Some(m as _);
                self.ahead -= m;

                (n as u8) << 1 | 1
            } else {
                self.next()? << 1
            }
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use alloc::vec::Vec;
    use core::iter::repeat;
    use pretty_assertions::assert_eq;

    const WINDOW_SIZE: usize = 8;
    const BUFFER_SIZE: usize = WINDOW_SIZE + MAX_LENGTH;

    #[test]
    fn next() {
        let mut iterator = LzssCompressionIterator::<BUFFER_SIZE, _>::new([1, 2, 3].into_iter());

        assert_eq!(iterator.next(), Some(1));
        assert_eq!(iterator.next(), Some(2));
        assert_eq!(iterator.next(), Some(3));
        assert_eq!(iterator.next(), None);
    }

    #[test]
    fn peek() {
        let mut iterator =
            LzssCompressionIterator::<BUFFER_SIZE, _>::new([1, 2, 3, 4, 5, 6].into_iter());

        assert_eq!(iterator.peek(0), Some(1));

        assert_eq!(iterator.peek(0), Some(1));
        assert_eq!(iterator.peek(1), Some(2));

        assert_eq!(iterator.peek(0), Some(1));
        assert_eq!(iterator.peek(1), Some(2));
        assert_eq!(iterator.peek(2), Some(3));

        assert_eq!(iterator.peek(0), Some(1));
        assert_eq!(iterator.peek(1), Some(2));
        assert_eq!(iterator.peek(2), Some(3));
        assert_eq!(iterator.peek(3), Some(4));
    }

    #[test]
    fn byte() {
        assert_eq!(
            LzssCompressionIterator::<BUFFER_SIZE, _>::new([1].into_iter()).collect::<Vec<_>>(),
            [2]
        );
    }

    #[test]
    fn two_bytes() {
        assert_eq!(
            LzssCompressionIterator::<BUFFER_SIZE, _>::new([1, 2].into_iter()).collect::<Vec<_>>(),
            [2, 4]
        );
    }

    #[test]
    fn three_bytes() {
        assert_eq!(
            LzssCompressionIterator::<BUFFER_SIZE, _>::new([1, 2, 3].into_iter())
                .collect::<Vec<_>>(),
            [2, 4, 6]
        );
    }

    #[test]
    fn two_repetitions() {
        assert_eq!(
            LzssCompressionIterator::<BUFFER_SIZE, _>::new([42, 42].into_iter())
                .collect::<Vec<_>>(),
            [84, 84]
        );
    }

    #[test]
    fn three_repetitions() {
        assert_eq!(
            LzssCompressionIterator::<BUFFER_SIZE, _>::new([42, 42, 42].into_iter())
                .collect::<Vec<_>>(),
            [84, 84, 84]
        );
    }

    #[test]
    fn four_repetitions() {
        assert_eq!(
            LzssCompressionIterator::<BUFFER_SIZE, _>::new([42, 42, 42, 42].into_iter())
                .collect::<Vec<_>>(),
            [84, 1, 3]
        );
    }

    #[test]
    fn repetitions() {
        assert_eq!(
            LzssCompressionIterator::<BUFFER_SIZE, _>::new(
                [42, 42, 42, 42, 7, 7, 7, 127, 127, 127, 127, 127].into_iter()
            )
            .collect::<Vec<_>>(),
            [84, 1, 3, 14, 14, 14, 254, 1, 4]
        );
    }

    #[test]
    fn uninitialized_zeros() {
        assert_eq!(
            LzssCompressionIterator::<BUFFER_SIZE, _>::new([0, 0, 0].into_iter())
                .collect::<Vec<_>>(),
            [15, 3]
        );
    }

    #[test]
    fn max_length() {
        assert_eq!(
            LzssCompressionIterator::<{ 1 + MAX_LENGTH }, _>::new(repeat(42).take(256))
                .collect::<Vec<_>>(),
            [84, 1, 255]
        );
    }

    #[test]
    fn max_offset() {
        assert_eq!(
            LzssCompressionIterator::<{ 128 + MAX_LENGTH }, _>::new((0..128).chain(0..128))
                .collect::<Vec<_>>(),
            (0..128)
                .map(|x| x << 1)
                .chain([255, 128])
                .collect::<Vec<_>>()
        );
    }
}
