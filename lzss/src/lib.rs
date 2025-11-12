//! LZSS compression.

#![no_std]

const MIN_MATCH: usize = 2;

/// LZSS compression.
pub trait Lzss: IntoIterator<Item = u8> {
    /// Compresses bytes.
    fn compress<const N: usize, const M: usize>(self) -> LzssCompressionIterator<M>;
    /// Decompresses bytes.
    fn decompress<const N: usize, const M: usize>(self) -> LzssDecompressionIterator<M>;
}

/// LZSS compression iterator.
pub struct LzssCompressionIterator<const M: usize> {
    window: [u8; M],
}

/// LZSS decompression iterator.
pub struct LzssDecompressionIterator<const M: usize> {
    window: [u8; M],
}

impl<I: IntoIterator<Item = u8>> Lzss for I {
    fn compress<const N: usize, const L: usize>(self) -> LzssCompressionIterator<M> {
        let xs = self.into_iter();
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
        //
        // ys

        todo!()
    }

    fn decompress(self) -> impl Iterator<Item = u8> {
        let mut ys = vec![];
        let mut i = 0;

        while let Some(&x) = xs.get(i) {
            if x.is_multiple_of(2) {
                ys.push(x >> 1);
            } else {
                for _ in 0..xs[i + 1] {
                    ys.push(ys[ys.len() - (x >> 1) as usize]);
                }
            }

            i += 1 + x as usize % 2;
        }

        ys
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        let data = b"ABABABABABABABABABABA123123123123";
        let compressed = compress::<64, 256>(data);

        assert_eq!(decompress(&compressed), data);
    }
}
