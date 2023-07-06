use std::process::exit;
use vm::{Error, FixedBufferDevice, Vm};

const HEAP_SIZE: usize = 1 << 8;

fn main() {
    if let Err(error) = run() {
        eprintln!("{}", error);
        exit(1);
    }
}

fn run() -> Result<(), Error> {
    Vm::<HEAP_SIZE, FixedBufferDevice<0, 0>>::new(Default::default())?.run()
}
