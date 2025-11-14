//! LZSS compression.

// TODO
// #![no_std]
#![allow(dead_code)]

#[cfg(test)]
extern crate alloc;
#[cfg(test)]
extern crate std;

mod ring_buffer;

use self::ring_buffer::RingBuffer;

const MINIMUM_LENGTH: usize = 2;
const MAXIMUM_LENGTH: usize = u8::MAX as _;

/// LZSS compression iterator.
pub struct LzssCompressionIterator<const W: usize, I: Iterator<Item = u8>> {
    iterator: I,
    // TODO Specify a separate buffer size for look-ahead?
    buffer: RingBuffer<W>,
    look_ahead: usize,
    next: Option<u8>,
}

impl<const W: usize, I: Iterator<Item = u8>> LzssCompressionIterator<W, I> {
    fn next(&mut self) -> Option<u8> {
        if self.look_ahead > 0 {
            let x = self.buffer.get(W - 1 - self.look_ahead);
            self.look_ahead -= 1;
            x
        } else if let Some(x) = self.iterator.next() {
            self.buffer.push(x);
            Some(x)
        } else {
            None
        }
    }

    fn peek(&mut self, index: usize) -> Option<u8> {
        if self.look_ahead > 0 {
            self.buffer.get(W - self.look_ahead - 1 + index)
        } else if let Some(x) = self.iterator.next() {
            self.buffer.push(x);
            Some(x)
        } else {
            None
        }
    }
}

impl<const W: usize, I: Iterator<Item = u8>> Iterator for LzssCompressionIterator<W, I> {
    type Item = u8;

    fn next(&mut self) -> Option<Self::Item> {
        Some(if let Some(x) = self.next {
            self.next = None;
            x
        } else {
            // TODO Prevent reading uninitialized bytes in a buffer?
            let (n, m) = (0..W)
                .map(|i| {
                    let mut j = 0;

                    while {
                        if j >= self.look_ahead
                            && let Some(x) = self.iterator.next()
                        {
                            self.buffer.push(x);
                            self.look_ahead += 1;

                            j < MAXIMUM_LENGTH
                                && self.buffer.get(W - self.look_ahead - i + j)
                                    == self.buffer.get(W - self.look_ahead + j)
                        } else {
                            false
                        }
                    } {
                        j += 1;
                    }

                    (i, j)
                })
                .max_by_key(|(_, j)| *j)
                .unwrap_or_default();

            if m > MINIMUM_LENGTH {
                self.next = Some(m as _);

                (n as u8) << 1 | 1
            } else {
                self.look_ahead -= 1;
                self.buffer.get(W - self.look_ahead)? << 1
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
        LzssCompressionIterator {
            iterator: self.into_iter(),
            buffer: RingBuffer::<W>::default(),
            look_ahead: 0,
            next: None,
        }
    }

    fn decompress<const W: usize>(self) -> impl Iterator<Item = u8> {
        LzssDecompressionIterator {
            iterator: self.into_iter(),
            buffer: RingBuffer::<W>::default(),
            offset: 0,
            length: 0,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use alloc::vec::Vec;
    use pretty_assertions::assert_eq;

    const WINDOW_SIZE: usize = 64;

    #[test]
    fn compress_and_decompress() {
        let data = b"ABABABABABABABABABABA123123123123";

        assert_eq!(
            data.iter()
                .copied()
                .compress::<WINDOW_SIZE>()
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
            let mut iterator = [1, 2, 3].iter().copied().compress::<WINDOW_SIZE>();

            assert_eq!(iterator.next(), Some(1));
            assert_eq!(iterator.next(), Some(2));
            assert_eq!(iterator.next(), Some(3));
            assert_eq!(iterator.next(), None);
        }

        #[test]
        fn byte() {
            assert_eq!(
                [1].iter()
                    .copied()
                    .compress::<WINDOW_SIZE>()
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
                    .compress::<WINDOW_SIZE>()
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
                    .compress::<WINDOW_SIZE>()
                    .collect::<Vec<_>>(),
                [2, 4, 6]
            );
        }

        #[test]
        fn repetition() {
            assert_eq!(
                [42, 42, 42]
                    .iter()
                    .copied()
                    .compress::<WINDOW_SIZE>()
                    .collect::<Vec<_>>(),
                [42, 0, 2]
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
