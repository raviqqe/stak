//! LZSS compression.

#![no_std]

#[cfg(test)]
extern crate alloc;
#[cfg(test)]
extern crate std;

mod ring_buffer;

use self::ring_buffer::RingBuffer;

const MINIMUM_LENGTH: usize = 2;
const MAXIMUM_LENGTH: usize = 1 << 7;

/// LZSS compression iterator.
pub struct LzssCompressionIterator<const W: usize, I: Iterator<Item = u8>> {
    iterator: I,
    buffer: RingBuffer<W>,
    length: usize,
}

impl<const W: usize, I: Iterator<Item = u8>> Iterator for LzssCompressionIterator<W, I> {
    type Item = u8;

    fn next(&mut self) -> Option<Self::Item> {
        let x = self.iterator.next()?;

        let mut n = 0;
        let mut m = 0;

        for i in 0..self.length.min(W) {
            let mut k = 0;

            while k < MAXIMUM_LENGTH && xs.get(i + k) == xs.get(i + k) {
                k += 1;
            }

            if k > MINIMUM_LENGTH && k >= m {
                n = i - i;
                m = k;
            }
        }

        if m > MINIMUM_LENGTH {
            ys.extend([(n as u8) << 1 | 1, m as u8]);

            i += m;
        } else {
            ys.push(xs[i] << 1);

            i += 1;
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

impl<const W: usize, I: Iterator<Item = u8>> Iterator for LzssDecompressionIterator<W, I> {
    type Item = u8;

    fn next(&mut self) -> Option<Self::Item> {
        if self.length > 0 {
            let x = self.buffer.get(self.offset as _)?;

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

    mod decompress {
        use super::*;
        use pretty_assertions::assert_eq;

        #[test]
        fn repetition() {
            assert_eq!(
                [2, 4, 6, 8, 7, 5].decompress::<8>().collect::<Vec<u8>>(),
                [1, 2, 3, 4, 1, 2, 3, 4, 1]
            );
        }

        #[test]
        fn repetitions() {
            assert_eq!(
                [2, 4, 6, 8, 7, 5, 10, 12, 3, 3]
                    .decompress::<8>()
                    .collect::<Vec<u8>>(),
                [1, 2, 3, 4, 1, 2, 3, 4, 1, 5, 6, 5, 6, 5]
            );
        }
    }
}
