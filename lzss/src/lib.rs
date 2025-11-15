//! LZSS compression.

// TODO
// #![no_std]
#![allow(dead_code, unused)]

#[cfg(test)]
extern crate alloc;
#[cfg(test)]
extern crate std;

mod ring_buffer;

use self::ring_buffer::RingBuffer;

const MINIMUM_LENGTH: usize = 3;
const MAXIMUM_LENGTH: usize = u8::MAX as _;

/// LZSS compression iterator.
pub struct LzssCompressionIterator<const B: usize, I: Iterator<Item = u8>> {
    iterator: I,
    // TODO Specify a separate buffer size for look-ahead?
    buffer: RingBuffer<B>,
    ahead: usize,
    next: Option<u8>,
}

impl<const B: usize, I: Iterator<Item = u8>> LzssCompressionIterator<B, I> {
    const WINDOW_SIZE: usize = B - MAXIMUM_LENGTH;

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
        if let Some(x) = self.next {
            self.next = None;
            Some(x)
        } else {
            // TODO Prevent reading uninitialized bytes in a buffer?
            let (n, m) = (0..Self::WINDOW_SIZE)
                .map(|i| {
                    let mut j = 0;

                    dbg!(
                        self.ahead,
                        1,
                        i,
                        self.buffer.get(2 * B - self.ahead - 1 - i + j),
                        self.peek(j)
                    );

                    while j < MAXIMUM_LENGTH
                        && self.buffer.get(2 * B - self.ahead - 1 - i + j) == self.peek(j)
                    {
                        j += 1;
                    }

                    (i, j)
                })
                .max_by_key(|(_, j)| *j)
                .unwrap_or_default();

            if m > MINIMUM_LENGTH {
                self.next = Some(m as _);

                Some((n as u8) << 1 | 1)
            } else {
                Some(self.next()? << 1)
            }
        }
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
    const BUFFER_SIZE: usize = WINDOW_SIZE + MAXIMUM_LENGTH;

    // #[test]
    // fn compress_and_decompress() {
    //     let data = b"ABABABABABABABABABABA123123123123";
    //
    //     assert_eq!(
    //         data.iter()
    //             .copied()
    //             .compress::<BUFFER_SIZE>()
    //             .decompress::<WINDOW_SIZE>()
    //             .collect::<Vec<u8>>(),
    //         data
    //     );
    // }

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

        // #[test]
        // fn four_repetitions() {
        //     assert_eq!(
        //         [42, 42, 42, 42]
        //             .iter()
        //             .copied()
        //             .compress::<BUFFER_SIZE>()
        //             .collect::<Vec<_>>(),
        //         [42, 0, 3]
        //     );
        // }

        // #[test]
        // fn repetitions() {
        //     assert_eq!(
        //         [42, 42, 42, 42, 7, 7, 7]
        //             .iter()
        //             .copied()
        //             .compress::<BUFFER_SIZE>()
        //             .collect::<Vec<_>>(),
        //         [42, 0, 3, 7, 0, 2]
        //     );
        // }
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
