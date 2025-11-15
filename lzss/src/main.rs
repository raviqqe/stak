//! An LZSS compressor for ASCII characters.

use clap::Parser;
use stak_lzss::{Lzss, MAX_LENGTH};
use std::error::Error;
use std::io::{Read, Write, stdin, stdout};

const WINDOW_SIZE: usize = 127;

#[derive(clap::Parser)]
#[command(about, version)]
struct Arguments {
    #[command(subcommand)]
    command: Command,
}

#[derive(clap::Subcommand)]
enum Command {
    /// Compresses data.
    Compress,
    /// Decompresses data.
    Decompress,
}

fn main() -> Result<(), Box<dyn Error>> {
    match Arguments::parse() {
        Command::Compress => compress(),
        Command::Decompress => decompress(),
    }
}

fn compress() -> Result<(), io::Error> {
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

fn decompress() -> Result<(), io::Error> {
    let mut data = vec![];

    stdin().read_to_end(&mut data)?;
    stdout().write(
        &data
            .into_iter()
            .decompress::<WINDOW_SIZE >()
            .collect::<Vec<_>>(),
    )?;

    Ok(())
}
