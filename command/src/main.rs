use device::StdioDevice;
use std::{env::args, error::Error, fs::read, process::exit};
use vm::{Number, Vm};

// TODO Change this value through an environment variable.
// TODO Make a default value `1 << 17`.
const HEAP_SIZE: usize = 1 << 20;

fn main() {
    if let Err(error) = run() {
        eprintln!("{}", error);
        exit(1);
    }
}

fn run() -> Result<(), Box<dyn Error>> {
    let mut heap = vec![Number::new(0).into(); HEAP_SIZE];
    let mut vm = Vm::<StdioDevice>::new(&mut heap, Default::default());

    vm.initialize(read(args().nth(1).ok_or(format!(
        "Usage: {} <bytecode_file>",
        args().next().expect("command name")
    ))?)?)?;

    Ok(vm.run()?)
}
