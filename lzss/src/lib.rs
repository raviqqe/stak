//! LZSS compression.

#![no_std]

#[cfg(test)]
extern crate alloc;
#[cfg(test)]
extern crate std;

mod ring_buffer;

use self::ring_buffer::RingBuffer;

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

    fn new(iterator: I) -> Self {
        Self {
            iterator,
            buffer: Default::default(),
            ahead: Default::default(),
            next: Default::default(),
        }
    }

    fn next(&mut self) -> Option<u8> {
        if self.ahead > 0 {
            let x = self.buffer.get(B - self.ahead);
            self.ahead -= 1;
            x
        } else if let Some(x) = self.iterator.next() {
            self.buffer.push(x);
            Some(x)
        } else {
            None
        }
    }

    fn peek(&mut self, index: usize) -> Option<u8> {
        if index < self.ahead {
            return self.buffer.get(B - self.ahead + index);
        }

        let mut x = 0;

        for _ in 0..index + 1 - self.ahead {
            x = self.iterator.next()?;
            self.buffer.push(x);
            self.ahead += 1;
        }

        Some(x)
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
                        && self.buffer.get(2 * B - self.ahead - 1 - i + j) == self.peek(j)
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
            buffer: RingBuffer::<W>::default(),
            offset: 0,
            length: 0,
        }
    }
}

impl<const W: usize, I: Iterator<Item = u8>> Iterator for LzssDecompressionIterator<W, I> {
    type Item = u8;

    fn next(&mut self) -> Option<Self::Item> {
        if self.length > 0 {
            let x = self
                .buffer
                .get(self.buffer.len() - 1 - self.offset as usize)?;

            self.buffer.push(x);
            self.length -= 1;

            Some(x)
        } else {
            let x = self.iterator.next()?;

            if x.is_multiple_of(2) {
                let x = x >> 1;
                self.buffer.push(x);
                Some(x)
            } else {
                self.offset = x >> 1;
                self.length = self.iterator.next()?;

                self.next()
            }
        }
    }
}

/// LZSS compression for 7-bit bytes.
pub trait Lzss {
    /// Compresses bytes.
    fn compress<const W: usize>(self) -> impl Iterator<Item = u8>;

    /// Decompresses bytes.
    fn decompress<const W: usize>(self) -> impl Iterator<Item = u8>;
}

impl<I: IntoIterator<Item = u8>> Lzss for I {
    fn compress<const W: usize>(self) -> impl Iterator<Item = u8> {
        LzssCompressionIterator::<W, _>::new(self.into_iter())
    }

    fn decompress<const W: usize>(self) -> impl Iterator<Item = u8> {
        LzssDecompressionIterator::<W, _>::new(self.into_iter())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use alloc::vec::Vec;
    use pretty_assertions::assert_eq;

    const WINDOW_SIZE: usize = 8;
    const BUFFER_SIZE: usize = WINDOW_SIZE + MAX_LENGTH;

    #[test]
    fn compress_and_decompress() {
        let data = b"ABABABABABABABABABABA123123123123";

        assert_eq!(
            data.iter()
                .copied()
                .compress::<BUFFER_SIZE>()
                .decompress::<WINDOW_SIZE>()
                .collect::<Vec<u8>>(),
            data
        );
    }

    #[test]
    fn compress_and_decompress_uninitialized_zeros() {
        let data = [0, 0, 0, 0, 0, 0];

        assert_eq!(
            data.iter()
                .copied()
                .compress::<BUFFER_SIZE>()
                .decompress::<WINDOW_SIZE>()
                .collect::<Vec<u8>>(),
            data
        );
    }

    mod compress {
        use super::*;
        use pretty_assertions::assert_eq;

        #[test]
        fn next() {
            let mut iterator =
                LzssCompressionIterator::<BUFFER_SIZE, _>::new([1, 2, 3].into_iter());

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
                [1].iter()
                    .copied()
                    .compress::<BUFFER_SIZE>()
                    .collect::<Vec<_>>(),
                [2]
            );
        }

        #[test]
        fn two_bytes() {
            assert_eq!(
                [1, 2]
                    .iter()
                    .copied()
                    .compress::<BUFFER_SIZE>()
                    .collect::<Vec<_>>(),
                [2, 4]
            );
        }

        #[test]
        fn three_bytes() {
            assert_eq!(
                [1, 2, 3]
                    .iter()
                    .copied()
                    .compress::<BUFFER_SIZE>()
                    .collect::<Vec<_>>(),
                [2, 4, 6]
            );
        }

        #[test]
        fn two_repetitions() {
            assert_eq!(
                [42, 42]
                    .iter()
                    .copied()
                    .compress::<BUFFER_SIZE>()
                    .collect::<Vec<_>>(),
                [84, 84]
            );
        }

        #[test]
        fn three_repetitions() {
            assert_eq!(
                [42, 42, 42]
                    .iter()
                    .copied()
                    .compress::<BUFFER_SIZE>()
                    .collect::<Vec<_>>(),
                [84, 84, 84]
            );
        }

        #[test]
        fn four_repetitions() {
            assert_eq!(
                [42, 42, 42, 42]
                    .iter()
                    .copied()
                    .compress::<BUFFER_SIZE>()
                    .collect::<Vec<_>>(),
                [84, 1, 3]
            );
        }

        #[test]
        fn repetitions() {
            assert_eq!(
                [42, 42, 42, 42, 7, 7, 7, 127, 127, 127, 127, 127]
                    .iter()
                    .copied()
                    .compress::<BUFFER_SIZE>()
                    .collect::<Vec<_>>(),
                [84, 1, 3, 14, 14, 14, 254, 1, 4]
            );
        }

        #[test]
        fn uninitialized_zeros() {
            assert_eq!(
                [0, 0, 0]
                    .iter()
                    .copied()
                    .compress::<BUFFER_SIZE>()
                    .collect::<Vec<_>>(),
                [15, 3]
            );
        }
    }

    mod decompress {
        use super::*;
        use pretty_assertions::assert_eq;

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
    }
}
