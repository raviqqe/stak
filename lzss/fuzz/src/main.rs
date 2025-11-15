//! A command to compress and decompress data by LZSS for fuzz testing.

use afl::fuzz;
use stak_lzss::{Lzss, MAX_LENGTH};

const WINDOW_SIZE: usize = 8;

fn main() {
    fuzz!(|data: &[u8]| {
        let data = data.iter().copied().map(|x| x >> 1).collect::<Vec<_>>();

        assert_eq!(
            data.iter()
                .copied()
                .compress::<{ WINDOW_SIZE + MAX_LENGTH }>()
                .decompress::<WINDOW_SIZE>()
                .collect::<Vec<_>>(),
            data
        );
    });
}
