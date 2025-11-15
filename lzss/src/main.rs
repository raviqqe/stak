//! An LZSS compressor for ASCII characters.

use stak_lzss::{Lzss, MAX_LENGTH};
use std::error::Error;
use std::io::{Read, Write, stdin, stdout};

const WINDOW_SIZE: usize = 256;

fn main() -> Result<(), Box<dyn Error>> {
    let mut data = vec![];

    stdin().read_to_end(&mut data)?;
    stdout().write(
        &data
            .into_iter()
            .compress::<{ WINDOW_SIZE + MAX_LENGTH }>()
            .collect::<Vec<_>>(),
    )?;

    Ok(())
}
