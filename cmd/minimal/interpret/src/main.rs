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
use mstak_util::Mmap;
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
    let [_, file] = slice::from_raw_parts(argv, argc as _) else {
        return 1;
    };

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

    let mmap = Mmap::new(*file);

    vm.initialize(mmap.as_slice().iter().copied()).unwrap();
    vm.run().unwrap();

    0
}
