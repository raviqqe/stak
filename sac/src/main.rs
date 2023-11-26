use device::StdioDevice;
use std::{env, error::Error, process::exit};
use vm::Vm;

const DEFAULT_HEAP_SIZE: usize = 1 << 21;

fn main() {
    if let Err(error) = run() {
        eprintln!("{}", error);
        exit(1);
    }
}

fn run() -> Result<(), Box<dyn Error>> {
    let size = env::var("STAK_HEAP_SIZE")
        .ok()
        .map(|string| string.parse())
        .transpose()?
        .unwrap_or(DEFAULT_HEAP_SIZE);
    let mut heap = vec![Default::default(); size];
    let mut vm = Vm::<StdioDevice>::new(&mut heap, Default::default());

    vm.initialize(include_bytes!(env!("STAK_BYTECODE_FILE")).iter().copied())?;

    Ok(vm.run()?)
}
