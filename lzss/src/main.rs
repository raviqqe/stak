use stak_lzss::Lzss;
use std::io::{Write, stdin, stdout};
use std::error::Error;

fn main() -> Result<(), Box<dyn Error>>{
    stdout().write(stdin().read()?.compress())
}
