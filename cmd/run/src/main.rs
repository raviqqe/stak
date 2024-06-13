//! A Stak Scheme interpreter.
//!
//! # Usage
//!
//! ```sh
//! stak foo.scm
//! ```

use clap::Parser;
use main_error::MainError;
use stak_configuration::DEFAULT_HEAP_SIZE;
use stak_device::{ReadWriteDevice, StdioDevice};
use stak_file::{LibcFileSystem, VoidFileSystem};
use stak_macro::include_r7rs;
use stak_minifier_macro::include_minified;
use stak_primitive::SmallPrimitiveSet;
use stak_vm::{Value, Vm};
use std::{
    error::Error,
    fs::File,
    io,
    io::{empty, Read},
    path::PathBuf,
};

const PRELUDE_SOURCE: &str = include_minified!("prelude.scm");
const COMPILER_PROGRAM: &[u8] = include_r7rs!("compile.scm");

#[derive(Clone, Copy, clap::ValueEnum)]
enum Library {
    None,
    R7rs,
}

#[derive(clap::Parser)]
#[command(about, version)]
struct Arguments {
    #[arg(required(true))]
    files: Vec<PathBuf>,
    #[arg(short = 's', long, default_value_t = DEFAULT_HEAP_SIZE)]
    heap_size: usize,
    #[arg(short, long, default_value = "r7rs")]
    library: Library,
}

fn main() -> Result<(), MainError> {
    let arguments = Arguments::parse();

    let mut heap = vec![Default::default(); arguments.heap_size];

    let mut source = match arguments.library {
        Library::None => Default::default(),
        Library::R7rs => PRELUDE_SOURCE.into(),
    };
    let mut target = vec![];

    read_source(&arguments.files, &mut source)?;
    compile(&source, &mut target, &mut heap)?;

    let mut vm = Vm::new(
        &mut heap,
        SmallPrimitiveSet::new(StdioDevice::new(), LibcFileSystem::new()),
    )?;

    vm.initialize(target)?;

    Ok(vm.run()?)
}

fn read_source(files: &[PathBuf], source: &mut String) -> Result<(), io::Error> {
    for file in files {
        File::open(file)?.read_to_string(source)?;
    }

    Ok(())
}

fn compile(source: &str, target: &mut Vec<u8>, heap: &mut [Value]) -> Result<(), Box<dyn Error>> {
    let mut vm = Vm::new(
        heap,
        SmallPrimitiveSet::new(
            ReadWriteDevice::new(source.as_bytes(), target, empty()),
            VoidFileSystem::new(),
        ),
    )?;

    vm.initialize(COMPILER_PROGRAM.iter().copied())?;

    Ok(vm.run()?)
}
