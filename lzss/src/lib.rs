//! LZSS compression.

#![no_std]
// TODO
#![allow(dead_code)]

#[cfg(test)]
extern crate alloc;

mod ring_buffer;

use self::ring_buffer::RingBuffer;

const MIN_MATCH: usize = 2;

/// LZSS compression iterator.
pub struct LzssCompressionIterator<const W: usize, I: Iterator<Item = u8>> {
    iterator: I,
    buffer: RingBuffer<W>,
}

impl<const W: usize, I: Iterator<Item = u8>> Iterator for LzssCompressionIterator<W, I> {
    type Item = u8;

    fn next(&mut self) -> Option<Self::Item> {
        // let xs = self.into_iter();
        // let mut ys = vec![];
        // let mut i = 0;
        //
        // while i < xs.len() {
        //     let mut n = 0;
        //     let mut m = 0;
        //
        //     for j in i.saturating_sub(N)..i {
        //         let mut k = 0;
        //
        //         while k < L && xs.get(i + k) == xs.get(j + k) {
        //             k += 1;
        //         }
        //
        //         if k >= MIN_MATCH && k >= m {
        //             n = i - j;
        //             m = k;
        //         }
        //     }
        //
        //     if m > MIN_MATCH {
        //         ys.extend([(n as u8) << 1 | 1, m as u8]);
        //
        //         i += m;
        //     } else {
        //         ys.push(xs[i] << 1);
        //
        //         i += 1;
        //     }
        // }

        todo!()
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
            // TODO
            self.buffer.get(self.offset as usize)
        } else {
            let x = self.iterator.next()?;

            if x.is_multiple_of(2) {
                Some(x >> 1)
            } else {
                self.offset = x >> 1;
                self.length = self.iterator.next()?;
                self.next()
            }
        }
    }
}

/// LZSS compression.
pub trait Lzss: IntoIterator {
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

    const WINDOW_SIZE: usize = 64;

    #[test]
    fn test() {
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
}
