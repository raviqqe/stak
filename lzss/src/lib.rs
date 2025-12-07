//! LZSS compression.

#![no_std]

#[cfg(test)]
extern crate alloc;
#[cfg(test)]
extern crate std;

mod compress;
mod decompress;
mod ring_buffer;

pub use self::compress::MAX_LENGTH;
use self::{compress::LzssCompressionIterator, decompress::LzssDecompressionIterator};

/// The maximum window size.
pub const MAX_WINDOW_SIZE: usize = 1 << 7;

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

    const WINDOW_SIZE: usize = 8;
    const BUFFER_SIZE: usize = WINDOW_SIZE + MAX_LENGTH;

    #[test]
    fn empty() {
        let data = [];

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
    fn byte() {
        let data = [42];

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
    fn two_bytes() {
        let data = [1, 2];

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
    fn three_bytes() {
        let data = [1, 2, 3];

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
    fn ascii_string() {
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
    fn uninitialized_zeros() {
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

    #[test]
    fn max_length() {
        const WINDOW_SIZE: usize = 1;
        let data = repeat(42).take(256).collect::<Vec<_>>();

        assert_eq!(
            data.iter()
                .copied()
                .compress::<{ WINDOW_SIZE + MAX_LENGTH }>()
                .decompress::<WINDOW_SIZE>()
                .collect::<Vec<_>>(),
            data
        );
    }

    #[test]
    fn max_offset() {
        const WINDOW_SIZE: usize = 128;
        let data = (0..128).chain(0..128).collect::<Vec<_>>();

        assert_eq!(
            data.iter()
                .copied()
                .compress::<{ WINDOW_SIZE + MAX_LENGTH }>()
                .decompress::<WINDOW_SIZE>()
                .collect::<Vec<_>>(),
            data
        );
    }

    #[quickcheck]
    fn random(data: Vec<u8>) -> bool {
        let data = data.into_iter().map(|x| x >> 1).collect::<Vec<_>>();

        data.iter()
            .copied()
            .compress::<{ WINDOW_SIZE + MAX_LENGTH }>()
            .decompress::<WINDOW_SIZE>()
            .collect::<Vec<_>>()
            == data
    }
}
