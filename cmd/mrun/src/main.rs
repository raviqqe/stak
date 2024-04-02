//! A minimal Stak Scheme interpreter.
//!
//! # Usage
//!
//! ```sh
//! stak foo.scm
//! ```

#![no_std]
#![no_main]

use core::{mem::size_of, slice};
use stak_configuration::DEFAULT_HEAP_SIZE;
use stak_macro::include_r7rs;
use stak_minifier_macro::include_minified;
use stak_primitive::SmallPrimitiveSet;
use stak_vm::{Value, Vm};

const PRELUDE_SOURCE: &str = "";
const COMPILER_PROGRAM: &[u8] = &[];

extern crate alloc;
extern crate libc;

const DEFAULT_BUFFER_SIZE: usize = 2usize.pow(20);

#[no_mangle]
unsafe extern "C" fn main(_argc: isize, _argv: *const *const u8) -> isize {
    let size = DEFAULT_HEAP_SIZE * size_of::<Value>();
    let mut heap = slice::from_raw_parts_mut::<Value>(libc::malloc(size) as _, size);
    let mut target = libc::malloc(DEFAULT_BUFFER_SIZE);

    let file = libc::fopen("main.scm" as *const _ as _, "rb" as *const _ as _);
    libc::fseek(file, 0, libc::SEEK_END);
    let size = libc::ftell(file) as usize;
    libc::rewind(file);

    let source = libc::malloc(size + 1);
    libc::fread(source, size, 1, file);
    libc::fclose(file);

    let target_buffer = slice::from_raw_parts_mut(target as _, DEFAULT_BUFFER_SIZE);

    compile(
        slice::from_raw_parts(source as _, size),
        target_buffer,
        &mut heap,
    );

    let mut vm = Vm::new(&mut heap, SmallPrimitiveSet::new(StdioDevice::new())).unwrap();

    vm.initialize(
        slice::from_raw_parts(target as _, DEFAULT_BUFFER_SIZE)
            .iter()
            .copied(),
    )
    .unwrap();
    vm.run().unwrap();

    0
}

fn compile(source: &[u8], target: &mut [u8], heap: &mut [Value]) {
    let mut vm = Vm::new(
        heap,
        SmallPrimitiveSet::new(ReadWriteDevice::new(source, target, (&mut []) as &mut [u8])),
    )
    .unwrap();

    vm.initialize(COMPILER_PROGRAM.iter().copied()).unwrap();
    vm.run().unwrap()
}

// #[panic_handler]
// fn handle_panic(_info: &core::panic::PanicInfo) -> ! {
//     libc::exit(1);
// }
