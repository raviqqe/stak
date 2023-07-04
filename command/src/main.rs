use std::process::exit;
use vm::{Error, FixedBufferDevice, Vm};

const HEAP_SIZE: usize = 1 << 8;

fn main() {
    if let Err(error) = run() {
        match error {
            Error::ArgumentCount => eprintln!("invalid argument count"),
            Error::ConsExpected => eprintln!("cons expected"),
            Error::IllegalInstruction => todo!(),
            Error::IllegalPrimitive => todo!(),
            Error::NumberExpected => eprintln!("number expected"),
            Error::OutOfMemory => eprintln!("out of memory"),
            Error::StackUnderflow => eprintln!("stack underflow"),
        }

        exit(1);
    }
}

fn run() -> Result<(), Error> {
    Vm::<HEAP_SIZE, _>::new(FixedBufferDevice::<0, 0>::new())?.run()
}
