//! A minimal command to interpret a bytecode file.
//!
//! # Usage
//!
//! ```sh
//! mstak-interpret foo.bc
//! ```

#![no_std]
#![cfg_attr(not(test), no_main)]

use core::slice;
use stak_device::libc::{ReadWriteDevice, Stderr, Stdin, Stdout};
use stak_primitive::SmallPrimitiveSet;
use stak_vm::Vm;

#[cfg(not(test))]
#[panic_handler]
fn panic(_info: &core::panic::PanicInfo) -> ! {
    unsafe { libc::exit(1) }
}

#[cfg_attr(not(test), no_mangle)]
unsafe extern "C" fn main(argc: isize, argv: *const *const i8) -> isize {
    let arguments = slice::from_raw_parts(argv, argc as _);

    if arguments.len() != 2 {
        return 1;
    }

    let mut heap = [Default::default(); 1 << 18];

    let mut vm = Vm::new(
        &mut heap,
        SmallPrimitiveSet::new(ReadWriteDevice::new(
            Stdin::new(),
            Stdout::new(),
            Stderr::new(),
        )),
    )
    .unwrap();

    vm.initialize(read_file(arguments[1]).iter().copied())
        .unwrap();
    vm.run().unwrap();

    0
}

unsafe fn read_file(path: *const i8) -> &'static [u8] {
    let file = libc::fopen(path, c"rb" as *const _ as _);
    libc::fseek(file, 0, libc::SEEK_END);
    let size = libc::ftell(file) as usize;
    libc::rewind(file);

    let source = libc::malloc(size + 1);
    libc::fread(source, size, 1, file);
    libc::fclose(file);

    slice::from_raw_parts(source as _, size)
}
