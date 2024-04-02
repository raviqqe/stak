//! A minimal Stak Scheme interpreter.
//!
//! # Usage
//!
//! ```sh
//! stak foo.scm
//! ```

#![no_std]
#![no_main]

use stak_configuration::DEFAULT_HEAP_SIZE;
use stak_device::{ReadWriteDevice, StdioDevice};
use stak_macro::include_r7rs;
use stak_minifier_macro::include_minified;
use stak_primitive::{SmallError, SmallPrimitiveSet};
use stak_vm::{Value, Vm};

const PRELUDE_SOURCE: &str = include_minified!("prelude.scm");
const COMPILER_PROGRAM: &[u8] = include_r7rs!("compile.scm");

extern crate alloc;
extern crate libc;

#[no_mangle]
extern "C" fn main(_argc: isize, _argv: *const *const u8) -> isize {
    run().unwrap();

    0
}

#[panic_handler]
fn handle_panic(_info: &core::panic::PanicInfo) -> ! {
    loop {}
}

fn run() -> Result<(), Box<dyn Error>> {
    let arguments = Arguments::parse();

    let mut heap = vec![Default::default(); arguments.heap_size];

    let mut source = PRELUDE_SOURCE.into();
    let mut target = vec![];

    read_source(&arguments.files, &mut source)?;
    compile(&source, &mut target, &mut heap)?;

    let mut vm = Vm::new(&mut heap, SmallPrimitiveSet::new(StdioDevice::new()))?;

    vm.initialize(target)?;

    Ok(vm.run()?)
}

fn read_source(files: &[PathBuf], source: &mut String) -> Result<(), io::Error> {
    for file in files {
        File::open(file)?.read_to_string(source)?;
    }

    Ok(())
}

fn compile(source: &str, target: &mut Vec<u8>, heap: &mut [Value]) -> Result<(), SmallError> {
    let mut vm = Vm::new(
        heap,
        SmallPrimitiveSet::new(ReadWriteDevice::new(source.as_bytes(), target, empty())),
    )?;

    vm.initialize(COMPILER_PROGRAM.iter().copied())?;

    Ok(vm.run()?)
}
