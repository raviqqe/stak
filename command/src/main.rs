use std::process::exit;
use vm::{Error, Vm};

fn main() {
    if let Err(error) = Vm::new().run() {
        match error {
            Error::ArgumentCount => eprintln!("invalid argument count"),
            Error::IllegalInstruction => todo!(),
            Error::IllegalPrimitive => todo!(),
        }

        exit(1);
    }
}
