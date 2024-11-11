//! A command to decode a bytecode file.
//!
//! It decodes a program in bytecodes and prints it in Markdown. This command is
//! primarily for debugging.
//!
//! # Usage
//!
//! ```sh
//! stak-decode < foo.bc
//! ```

// TODO Rewrite in Scheme.

use main_error::MainError;
use stak_code::decode;
use std::io::{stdin, Read};

#[derive(clap::Parser)]
#[command(about, version)]
struct Arguments {}

fn main() -> Result<(), MainError> {
    let mut buffer = vec![];
    stdin().read_to_end(&mut buffer)?;

    println!("{}", decode(&buffer)?);

    Ok(())
}
