use std::{env::args, fs::read_to_string, process::exit};
use vm::{Error, Vm};

fn main() {
    let input = match read_to_string(args().nth(1).expect("command line argument")) {
        Ok(input) => input,
        Err(error) => {
            eprintln!("{}", error);
            return;
        }
    };

    if let Err(error) = Vm::new(input.trim().as_bytes()).run() {
        exit(match error {
            Error::IllegalInstruction | Error::IllegalPrimitive => 6,
            _ => 1,
        })
    }
}
