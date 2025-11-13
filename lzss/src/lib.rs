//! LZSS compression.

#![no_std]

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
    buffer: RingBuffer<W>,
    length: u8,
}

impl<const W: usize, I: Iterator<Item = u8>> Iterator for LzssCompressionIterator<W, I> {
    type Item = u8;

    fn next(&mut self) -> Option<Self::Item> {
        Some(if self.length > 0 {
            let x = self.length;
            self.length = 0;
            x
        } else {
            let x = self.iterator.next()?;
            let mut d = 0;

            // let mut n = 0;
            // let mut m = 0;
            // let mut d = 0;
            //
            // // TODO Prevent reading uninitialized bytes in a buffer?
            // for i in 0..W {
            //     let mut k = 0;
            //
            //     while {
            //         if k >= d
            //             && let Some(x) = self.iterator.next()
            //         {
            //             self.buffer.push(x);
            //             d += 1;
            //         }
            //
            //         let d = self.buffer.len() - d;
            //
            //         k < MAXIMUM_LENGTH
            //             && self.buffer.get(d + k) == self.buffer.get(d + self.buffer.len() - i + k)
            //     } {
            //         k += 1;
            //     }
            //
            //     // Prefer a smaller offset.
            //     if k > MINIMUM_LENGTH && k > m {
            //         n = i;
            //         m = k;
            //     }
            // }

            let (n, m) = (0..W)
                .map(|i| {
                    let mut j = 0;

                    while {
                        if j >= d
                            && let Some(x) = self.iterator.next()
                        {
                            self.buffer.push(x);
                            d += 1;
                        }

                        let d = self.buffer.len() - d;

                        j < MAXIMUM_LENGTH
                            && self.buffer.get(d + j)
                                == self.buffer.get(d + self.buffer.len() - i + j)
                    } {
                        j += 1;
                    }

                    (i, j)
                })
                .max_by_key(|(_, j)| *j)
                .unwrap_or_default();

            if m > MINIMUM_LENGTH {
                self.length = m as _;

                (n as u8) << 1 | 1
            } else {
                x << 1
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
            let x = self.buffer.get(self.buffer.len() - self.offset as usize)?;

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
            length: 0,
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
