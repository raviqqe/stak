use code::decode;
use std::{env::args, error::Error, fs::read, process::exit};

fn main() {
    if let Err(error) = run() {
        eprintln!("{}", error);
        exit(1);
    }
}

fn run() -> Result<(), Box<dyn Error>> {
    eprintln!(
        "{:#?}",
        decode(&read(args().nth(1).ok_or(format!(
            "Usage: {} <bytecode_file>",
            args().next().expect("command name")
        ))?)?)?
    );

    Ok(())
}
