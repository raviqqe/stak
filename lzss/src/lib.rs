//! LZSS compression.

#![no_std]

#[cfg(test)]
extern crate alloc;

const MIN_MATCH: usize = 2;

/// LZSS compression iterator.
pub struct LzssCompressionIterator<const W: usize, I: Iterator<Item = u8>> {
    iterator: I,
    buffer: [u8; W],
    index: usize,
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
    buffer: [u8; W],
    index: usize,
}

impl<const W: usize, I: Iterator<Item = u8>> Iterator for LzssDecompressionIterator<W, I> {
    type Item = u8;

    fn next(&mut self) -> Option<Self::Item> {
        // let mut ys = vec![];
        // let mut i = 0;
        //
        // while let Some(&x) = xs.get(i) {
        //     if x.is_multiple_of(2) {
        //         ys.push(x >> 1);
        //     } else {
        //         for _ in 0..xs[i + 1] {
        //             ys.push(ys[ys.len() - (x >> 1) as usize]);
        //         }
        //     }
        //
        //     i += 1 + x as usize % 2;
        // }

        todo!()
    }
}

/// LZSS compression.
pub trait Lzss: IntoIterator {
    /// Compresses bytes.
    fn compress<const W: usize>(self) -> LzssCompressionIterator<W, Self::IntoIter>;

    /// Decompresses bytes.
    fn decompress<const W: usize>(self) -> LzssDecompressionIterator<W, Self::IntoIter>;
}

impl<I: IntoIterator<Item = u8>> Lzss for I {
    fn compress<const W: usize>(self) -> LzssCompressionIterator<W, I::IntoIter> {
        todo!()
    }

    fn decompress<const W: usize>(self) -> LzssDecompressionIterator<W, I::IntoIter> {
        LzssDecompressionIterator {
            iterator: self.into_iter(),
            buffer: [0; W],
            index: 0,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use alloc::vec::Vec;

    #[test]
    fn test() {
        let data = b"ABABABABABABABABABABA123123123123";
        let compressed = data.iter().copied().compress::<64>();

        assert_eq!(
            compressed.decompress().collect::<Vec<u8>>().as_slice(),
            data
        );
    }
}
