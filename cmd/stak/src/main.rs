//! A Stak Scheme interpreter.
//! machine.
//!
//! # Usage
//!
//! ```sh
//! stak foo.scm
//! ```

use clap::Parser;
use stak_device::{ReadWriteDevice, StdioDevice};
use stak_macro::include_r7rs;
use stak_primitive::SmallPrimitiveSet;
use stak_vm::{Value, Vm};
use std::{
    env,
    error::Error,
    fs::File,
    io,
    io::{empty, Read},
};

const DEFAULT_HEAP_SIZE: usize = 1 << 21;
const COMPILER_PROGRAM: &[u8] = include_r7rs!("compile.scm");

#[derive(clap::ValueEnum)]
enum Library {
    R7rs,
}

#[derive(clap::Parser)]
#[command(about, version)]
struct Arguments {
    #[arg(short, long, default_value_t = Some(Library::R7rs))]
    library: Option<Library>,
}

fn main() -> Result<(), Box<dyn Error>> {
    let arguments = Arguments::parse();

    let size = env::var("STAK_HEAP_SIZE")
        .ok()
        .map(|string| string.parse())
        .transpose()?
        .unwrap_or(DEFAULT_HEAP_SIZE);
    let mut heap = vec![Default::default(); size];

    let mut source = String::new();
    let mut target = vec![];

    read_source(&mut source)?;
    compile(&source, &mut target, &mut heap)?;

    let mut vm = Vm::new(&mut heap, SmallPrimitiveSet::new(StdioDevice::new()))?;

    vm.initialize(target)?;

    Ok(vm.run()?)
}

fn read_source(source: &mut String) -> Result<(), io::Error> {
    for argument in env::args().skip(1) {
        File::open(argument)?.read_to_string(source)?;
    }

    Ok(())
}

fn compile(source: &str, target: &mut Vec<u8>, heap: &mut [Value]) -> Result<(), Box<dyn Error>> {
    let mut vm = Vm::new(
        heap,
        SmallPrimitiveSet::new(ReadWriteDevice::new(source.as_bytes(), target, empty())),
    )?;

    vm.initialize(COMPILER_PROGRAM.iter().copied())?;

    Ok(vm.run()?)
}
