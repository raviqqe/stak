use device::StdioDevice;
use std::{
    env::{self, args},
    error::Error,
    fs::read,
    process::exit,
};
use vm::{Number, Vm};

fn main() {
    if let Err(error) = run() {
        eprintln!("{}", error);
        exit(1);
    }
}

fn run() -> Result<(), Box<dyn Error>> {
    let size = env::var("STAK_HEAP_SIZE").parse()?;
    let mut heap = vec![Number::new(0).into(); size];
    let mut vm = Vm::<StdioDevice>::new(&mut heap, Default::default());

    vm.initialize(read(args().nth(1).ok_or(format!(
        "Usage: {} <bytecode_file>",
        args().next().expect("command name")
    ))?)?)?;

    Ok(vm.run()?)
}
