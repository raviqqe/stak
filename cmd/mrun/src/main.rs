//! A minimal Stak Scheme interpreter.
//!
//! # Usage
//!
//! ```sh
//! mstak foo.scm
//! ```

#![no_std]
#![cfg_attr(not(test), no_main)]

use core::{mem::size_of, slice};
use stak_configuration::DEFAULT_HEAP_SIZE;
use stak_device::libc::{
    Read, ReadBuffer, ReadWriteDevice, Stderr, Stdin, Stdout, Write, WriteBuffer,
};
use stak_primitive::SmallPrimitiveSet;
use stak_vm::{Value, Vm};

const PRELUDE_SOURCE: &str = include_str!("prelude.scm");
const COMPILER_PROGRAM: &[u8] = &[]; // TODO

const DEFAULT_BUFFER_SIZE: usize = 2usize.pow(20);

#[cfg(not(test))]
#[panic_handler]
fn panic(_info: &core::panic::PanicInfo) -> ! {
    unsafe { libc::exit(1) }
}

#[cfg_attr(not(test), no_mangle)]
unsafe extern "C" fn main(argc: isize, argv: *const *const u8) -> isize {
    let arguments = slice::from_raw_parts(argv, argc as _);

    if arguments.len() != 2 {
        return 1;
    }

    let source = read_file(arguments[1] as *const i8);

    let mut target = WriteBuffer::new(slice::from_raw_parts_mut(
        libc::malloc(DEFAULT_BUFFER_SIZE) as _,
        DEFAULT_BUFFER_SIZE,
    ));

    let heap = slice::from_raw_parts_mut::<Value>(
        libc::malloc(DEFAULT_HEAP_SIZE * size_of::<Value>()) as _,
        DEFAULT_HEAP_SIZE,
    );

    compile(source, &mut target, heap);

    let mut vm = Vm::new(
        heap,
        SmallPrimitiveSet::new(ReadWriteDevice::new(
            Stdin::new(),
            Stdout::new(),
            Stderr::new(),
        )),
    )
    .unwrap();

    vm.initialize(target.as_bytes().iter().copied()).unwrap();
    vm.run().unwrap();

    0
}

fn compile(source: impl Read, target: impl Write, heap: &mut [Value]) {
    let mut vm = Vm::new(
        heap,
        SmallPrimitiveSet::new(ReadWriteDevice::new(source, target, Stderr::new())),
    )
    .unwrap();

    vm.initialize(COMPILER_PROGRAM.iter().copied()).unwrap();
    vm.run().unwrap()
}

fn read_file(path: *const i8) -> ReadBuffer<'static> {
    unsafe {
        let file = libc::fopen(path, "rb" as *const _ as _);
        libc::fseek(file, 0, libc::SEEK_END);
        let size = libc::ftell(file) as usize;
        libc::rewind(file);

        let source = libc::malloc(size + 1);
        libc::fread(source, size, 1, file);
        libc::fclose(file);

        ReadBuffer::new(slice::from_raw_parts(source as _, size))
    }
}
