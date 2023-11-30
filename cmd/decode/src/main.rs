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
