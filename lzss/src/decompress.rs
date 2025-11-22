use crate::ring_buffer::RingBuffer;

const MIN_LENGTH: usize = 2;
/// The maximum match length.
pub const MAX_LENGTH: usize = u8::MAX as _;
/// The default window size.
pub const DEFAULT_WINDOW_SIZE: usize = (1 << 7) - 1;

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
