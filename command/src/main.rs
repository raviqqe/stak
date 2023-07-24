use std::{env::args, error::Error, fs::read_to_string, process::exit};
use vm::{FixedBufferDevice, Vm};

const HEAP_SIZE: usize = 1 << 9;

fn main() {
    if let Err(error) = run() {
        eprintln!("{}", error);
        exit(1);
    }
}

fn run() -> Result<(), Box<dyn Error>> {
    let mut vm = Vm::<HEAP_SIZE, FixedBufferDevice<0, 0>>::new(Default::default())?;

    vm.initialize(
        read_to_string(args().nth(1).ok_or(format!(
            "Usage: {} <bytecode_file>",
            args().next().expect("command name")
        ))?)?
        .as_bytes(),
    )?;

    Ok(vm.run()?)
}
