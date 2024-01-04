//! A command to decode a bytecode file.
//!
//! It decodes a program in bytecodes and prints it in Markdown. This command is primarily for
//! debugging.
//!
//! # Usage
//!
//! ```sh
//! stak-decode < foo.bc
//! ```

use code::decode;
use std::{
    error::Error,
    io::{stdin, Read},
    process::exit,
};

fn main() {
    if let Err(error) = run() {
        eprintln!("{}", error);
        exit(1);
    }
}

fn run() -> Result<(), Box<dyn Error>> {
    let mut buffer = vec![];
    stdin().read_to_end(&mut buffer)?;

    println!("{}", decode(&buffer)?);

    Ok(())
}
