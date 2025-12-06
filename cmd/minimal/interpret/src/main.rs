//! A minimal command to interpret a bytecode file.
//!
//! # Usage
//!
//! ```sh
//! mstak-interpret foo.bc
//! ```

#![no_std]
#![cfg_attr(not(test), no_main)]

extern crate alloc;

use alloc::alloc::alloc;
use core::{alloc::Layout, ffi::CStr, slice};
use dlmalloc::GlobalDlmalloc;
use origin::program::exit;
use stak_device::libc::{ReadWriteDevice, Stderr, Stdin, Stdout};
use stak_file::LibcFileSystem;
use stak_libc::Mmap;
use stak_process_context::LibcProcessContext;
use stak_r7rs::SmallPrimitiveSet;
use stak_time::LibcClock;
use stak_vm::{Value, Vm};

const HEAP_SIZE: usize = 1 << 19;

#[global_allocator]
static GLOBAL_ALLOCATOR: GlobalDlmalloc = GlobalDlmalloc;

#[cfg(not(test))]
#[panic_handler]
fn panic(_info: &core::panic::PanicInfo) -> ! {
    exit(1)
}

#[cfg_attr(not(test), unsafe(no_mangle))]
extern "C" fn main(argc: isize, argv: *const *const i8) {
    // SAFETY: Operating systems guarantee `argv` to have the length of `argc`.
    let Some(&file) = unsafe { slice::from_raw_parts(argv, argc as _) }.get(1) else {
        exit(1);
    };

    // SAFETY: `Value`s have `'static` lifetimes and can have any inner values of
    // unsigned integers.
    let heap = unsafe {
        slice::from_raw_parts_mut(
            alloc(Layout::array::<Value>(HEAP_SIZE).unwrap()) as *mut Value,
            HEAP_SIZE,
        )
    };

    let mut vm = Vm::new(
        heap,
        SmallPrimitiveSet::new(
            ReadWriteDevice::new(Stdin::new(), Stdout::new(), Stderr::new()),
            LibcFileSystem::new(),
            // SAFETY: `argc` and `argv` are provided by operating systems.
            unsafe { LibcProcessContext::new(argc - 1, argv.add(1)) },
            LibcClock::new(),
        ),
    )
    .unwrap();

    // SAFETY: `file` is from `argv` and guaranteed to have a C string.
    let mmap = Mmap::new(unsafe { CStr::from_ptr(file as _) }).unwrap();

    vm.run(mmap.as_slice().iter().copied()).unwrap();

    exit(0);
}
