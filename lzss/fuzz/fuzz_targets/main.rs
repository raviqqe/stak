#![no_main]

use libfuzzer_sys::fuzz_target;
use stak_lzss::{Lzss, MAX_LENGTH};

const WINDOW_SIZE: usize = 8;

fuzz_target!(|data: &[u8]| {
    let data = data.iter().copied().map(|x| x >> 1).collect::<Vec<_>>();

    assert_eq!(
        data.iter()
            .copied()
            .compress::<{ WINDOW_SIZE + MAX_LENGTH }>()
            .decompress::<WINDOW_SIZE>()
            .collect::<Vec<u8>>(),
        data
    );
});
