use std::process::exit;
use vm::{Error, Vm};

fn main() {
    if let Err(error) = Vm::new(b"").run() {
        match error {
            Error::ArgumentCount => todo!(),
            Error::IllegalInstruction => todo!(),
            Error::IllegalPrimitive => todo!(),
        }

        exit(1)
    }
}
