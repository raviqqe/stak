//! An LZSS compressor for ASCII characters.

extern crate alloc;

use alloc::vec;
use clap::Parser;
use core::error::Error;
use stak_lzss::{Lzss, MAX_LENGTH};
use std::io::{self, Read, Write, stdin, stdout};

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
    match Arguments::parse().command {
        Command::Compress => run(Lzss::compress::<{ WINDOW_SIZE + MAX_LENGTH }>)?,
        Command::Decompress => run(Lzss::decompress::<WINDOW_SIZE>)?,
    }

    Ok(())
}

fn run<I: Iterator<Item = u8>, F: Fn(vec::IntoIter<u8>) -> I>(convert: F) -> Result<(), io::Error> {
    let mut data = vec![];

    stdin().read_to_end(&mut data)?;
    stdout().write_all(&convert(data.into_iter()).collect::<Vec<_>>())?;

    Ok(())
}
