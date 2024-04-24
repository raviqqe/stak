//! A command to profile a bytecode file.
//!
//! # Usage
//!
//! ```sh
//! stak-profile --profile profile.txt foo.bc
//! ```

use clap::Parser;
use main_error::MainError;
use stak_configuration::DEFAULT_HEAP_SIZE;
use stak_device::StdioDevice;
use stak_primitive::SmallPrimitiveSet;
use stak_vm::{Cons, PrimitiveSet, Profiler, Type, Vm, FRAME_TAG};
use std::{
    fs::{read, OpenOptions},
    io::Write,
    path::PathBuf,
    time::Instant,
};

const SEPARATOR: char = '\t';

#[derive(clap::Parser)]
#[command(about, version)]
struct Arguments {
    #[arg(required(true))]
    bytecode_file: PathBuf,
    #[arg(short = 'p', long = "profile", required(true))]
    profile_file: PathBuf,
    #[arg(short = 's', long, default_value_t = DEFAULT_HEAP_SIZE)]
    heap_size: usize,
}

fn main() -> Result<(), MainError> {
    let arguments = Arguments::parse();
    let mut profile_file = OpenOptions::new()
        .write(true)
        .create(true)
        .truncate(true)
        .open(&arguments.profile_file)?;

    let mut profiler = WriteProfiler::new(profile_file);
    let mut heap = vec![Default::default(); arguments.heap_size];
    let mut vm = Vm::new(&mut heap, SmallPrimitiveSet::new(StdioDevice::new()))?
        .with_profiler(&mut profiler);

    vm.initialize(read(&arguments.bytecode_file)?)?;

    Ok(vm.run()?)
}

struct WriteProfiler<T: Write> {
    writer: T,
    start_time: Instant,
}

impl<T: Write> WriteProfiler<T> {
    pub fn new(writer: T) -> Self {
        Self {
            writer,
            start_time: Instant::now(),
        }
    }

    fn write(&mut self) {
        write!(
            &mut self.writer,
            "{}{}",
            if r#return { "return" } else { "call" },
            SEPARATOR,
        )
        .unwrap();

        let mut stack = vm.stack();

        while stack != vm.null() {
            if vm.cdr(stack).tag() == FRAME_TAG {
                let operand = vm.car_value(vm.car_value(vm.car(stack)));

                if let Some(symbol) = operand.to_cons() {
                    if vm.car(symbol).tag() != Type::Symbol as _ {
                        // TODO Remove this hack.
                        write!(profile_file, "<top>").unwrap();
                        break;
                    } else {
                        let mut string = vm.car_value(vm.car(symbol)).assume_cons();

                        while string != vm.null() {
                            write!(
                                profile_file,
                                "{}",
                                char::from_u32(vm.car(string).assume_number().to_i64() as _)
                                    .unwrap_or('�')
                            )
                            .unwrap();
                            string = vm.cdr(string).assume_cons();
                        }
                    }
                } else {
                    write!(profile_file, "<local>").unwrap();
                }

                write!(&mut self.writer, ";").unwrap();

                stack = vm.cdr_value(vm.car(stack)).assume_cons();
            } else {
                stack = vm.cdr(stack).assume_cons();
            }
        }

        writeln!(
            &mut self.writer,
            "{}{}",
            SEPARATOR,
            Instant::now().duration_since(self.start_time).as_nanos()
        )
        .unwrap();
    }
}

impl<T: Write, P: PrimitiveSet> Profiler for WriteProfiler<T, P> {
    fn profile_call(&mut self, vm: &Vm<P>, call_code: Cons) {}

    fn profile_return(&mut self, vm: &Vm<P>) {}
}
