use std::process::exit;
use vm::{Error, Vm};

const HEAP_SIZE: usize = 1 << 8;

fn main() {
    if let Err(error) = Vm::<HEAP_SIZE>::new().run() {
        match error {
            Error::ArgumentCount => eprintln!("invalid argument count"),
            Error::IllegalInstruction => todo!(),
            Error::IllegalPrimitive => todo!(),
            Error::OutOfMemory => eprintln!("out of memory"),
        }

        exit(1);
    }
}
