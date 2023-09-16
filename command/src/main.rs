use device::StdioDevice;
use std::{env::args, error::Error, fs::read, process::exit};
use vm::Vm;

const HEAP_SIZE: usize = 1 << 17;

fn main() {
    if let Err(error) = run() {
        eprintln!("{}", error);
        exit(1);
    }
}

fn run() -> Result<(), Box<dyn Error>> {
    let mut heap = [ZERO.into(); HEAP_SIZE];
    let mut vm = Vm::<StdioDevice>::new(&mut heap, Default::default())?;

    vm.initialize(read(args().nth(1).ok_or(format!(
        "Usage: {} <bytecode_file>",
        args().next().expect("command name")
    ))?)?)?;

    Ok(vm.run()?)
}
