//! A minimal Stak Scheme interpreter.
//!
//! # Usage
//!
//! ```sh
//! mstak foo.scm
//! ```

#![no_std]
#![cfg_attr(not(test), no_main)]

use core::{array, env, ffi::CStr, slice};
use mstak_util::{Heap, Mmap};
use stak_configuration::DEFAULT_HEAP_SIZE;
use stak_device::libc::{Buffer, BufferMut, Read, ReadWriteDevice, Stderr, Stdin, Stdout, Write};
use stak_file::{LibcFileSystem, VoidFileSystem};
use stak_primitive::SmallPrimitiveSet;
use stak_process_context::VoidProcessContext;
use stak_vm::{Value, Vm};

const PRELUDE_SOURCE: &str = include_str!(env!("STAK_PRELUDE_FILE"));
const COMPILER_BYTECODES: &[u8] = include_bytes!(env!("STAK_COMPILER_FILE"));

const DEFAULT_BUFFER_SIZE: usize = 1 << 18;
const MAX_SOURCE_FILE_COUNT: usize = 4;

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

    let mut mmaps = array::from_fn::<_, MAX_SOURCE_FILE_COUNT, _>(|_| None);

    for (index, &path) in arguments.iter().enumerate() {
        mmaps[index] = Some(Mmap::new(CStr::from_ptr(path)));
    }

    let mut sources = [Default::default(); MAX_SOURCE_FILE_COUNT + 1];

    sources[0] = PRELUDE_SOURCE.as_bytes();

    for (index, mmap) in mmaps.iter().enumerate() {
        if let Some(mmap) = mmap {
            sources[index + 1] = mmap.as_slice();
        }
    }

    let mut buffer = Heap::new(DEFAULT_BUFFER_SIZE, Default::default);
    let mut target = BufferMut::new(buffer.as_slice_mut());
    let mut heap = Heap::new(DEFAULT_HEAP_SIZE, Default::default);

    compile(Buffer::new(&sources), &mut target, heap.as_slice_mut());

    let mut vm = Vm::new(
        heap.as_slice_mut(),
        SmallPrimitiveSet::new(
            ReadWriteDevice::new(Stdin::new(), Stdout::new(), Stderr::new()),
            LibcFileSystem::new(),
            VoidProcessContext::new(),
        ),
    )
    .unwrap();

    vm.initialize(target.as_bytes().iter().copied()).unwrap();
    vm.run().unwrap();

    0
}

fn compile(source: impl Read, target: impl Write, heap: &mut [Value]) {
    let mut vm = Vm::new(
        heap,
        SmallPrimitiveSet::new(
            ReadWriteDevice::new(source, target, Stderr::new()),
            VoidFileSystem::new(),
            VoidProcessContext::new(),
        ),
    )
    .unwrap();

    vm.initialize(COMPILER_BYTECODES.iter().copied()).unwrap();
    vm.run().unwrap()
}
