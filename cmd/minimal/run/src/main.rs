//! A minimal Stak Scheme interpreter.
//!
//! # Usage
//!
//! ```sh
//! mstak foo.scm
//! ```

#![no_std]
#![cfg_attr(not(test), no_main)]

use core::{env, mem::size_of, ptr::null_mut, slice};
use stak_configuration::DEFAULT_HEAP_SIZE;
use stak_device::libc::{Buffer, BufferMut, Read, ReadWriteDevice, Stderr, Stdin, Stdout, Write};
use stak_primitive::SmallPrimitiveSet;
use stak_vm::{Value, Vm};

const PRELUDE_SOURCE: &str = include_str!(env!("STAK_PRELUDE_FILE"));
const COMPILER_BYTECODES: &[u8] = include_bytes!(env!("STAK_COMPILER_FILE"));

const DEFAULT_BUFFER_SIZE: usize = 2usize.pow(18);

#[cfg(not(test))]
#[panic_handler]
fn panic(_info: &core::panic::PanicInfo) -> ! {
    unsafe { libc::exit(1) }
}

#[cfg_attr(not(test), no_mangle)]
unsafe extern "C" fn main(argc: isize, argv: *const *const i8) -> isize {
    let arguments = &slice::from_raw_parts(argv, argc as _)[1..];

    if arguments.is_empty() {
        return 1;
    }

    let sources = [PRELUDE_SOURCE.as_bytes(), read_file(arguments[0])];
    let mut target = BufferMut::new(allocate_memory::<u8>(DEFAULT_BUFFER_SIZE));
    let heap = allocate_memory::<Value>(DEFAULT_HEAP_SIZE);

    compile(Buffer::new(&sources), &mut target, heap);

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

unsafe fn allocate_memory<'a, T>(size: usize) -> &'a mut [T] {
    slice::from_raw_parts_mut::<T>(libc::malloc(size * size_of::<Value>()) as _, size)
}

fn compile(source: impl Read, target: impl Write, heap: &mut [Value]) {
    let mut vm = Vm::new(
        heap,
        SmallPrimitiveSet::new(ReadWriteDevice::new(source, target, Stderr::new())),
    )
    .unwrap();

    vm.initialize(COMPILER_BYTECODES.iter().copied()).unwrap();
    vm.run().unwrap()
}

unsafe fn read_file(path: *const i8) -> &'static [u8] {
    let file = libc::fopen(path, c"rb" as *const _ as _);
    libc::fseek(file, 0, libc::SEEK_END);
    let size = libc::ftell(file) as usize;
    libc::fclose(file);

    slice::from_raw_parts(
        libc::mmap(null_mut(), 0, 0, 0, libc::open(path, libc::O_RDONLY), 0) as *const u8,
        size,
    )
}
