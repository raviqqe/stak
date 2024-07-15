//! A minimal command to interpret a bytecode file.
//!
//! # Usage
//!
//! ```sh
//! mstak-interpret foo.bc
//! ```

#![no_std]
#![cfg_attr(not(test), no_main)]

use core::{ffi::CStr, mem::size_of, slice};
use mstak_util::Mmap;
use stak_device::libc::{ReadWriteDevice, Stderr, Stdin, Stdout};
use stak_file::LibcFileSystem;
use stak_primitive::SmallPrimitiveSet;
use stak_process_context::LibcProcessContext;
use stak_vm::{Value, Vm};

const HEAP_SIZE: usize = 1 << 19;

#[cfg(not(test))]
#[panic_handler]
fn panic(_info: &core::panic::PanicInfo) -> ! {
    unsafe { libc::exit(1) }
}

#[cfg_attr(not(test), no_mangle)]
unsafe extern "C" fn main(argc: isize, argv: *const *const i8) -> isize {
    let Some(&file) = &slice::from_raw_parts(argv, argc as _).get(1) else {
        return 1;
    };

    let heap = slice::from_raw_parts_mut(
        libc::malloc(size_of::<Value>() * HEAP_SIZE) as *mut Value,
        HEAP_SIZE,
    );

    let mut vm = Vm::new(
        heap,
        SmallPrimitiveSet::new(
            ReadWriteDevice::new(Stdin::new(), Stdout::new(), Stderr::new()),
            LibcFileSystem::new(),
            LibcProcessContext::new(argc, argv),
        ),
    )
    .unwrap();

    let mmap = Mmap::new(CStr::from_ptr(file));

    vm.initialize(mmap.as_slice().iter().copied()).unwrap();
    vm.run().unwrap();

    0
}
