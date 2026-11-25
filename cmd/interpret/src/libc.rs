use crate::options::Options;
use alloc::boxed::Box;
use core::{ffi::CStr, fmt::Write};
use dlmalloc::GlobalDlmalloc;
use origin::program::exit;
use stak_configuration::DEFAULT_HEAP_SIZE;
use stak_device::libc::{ReadWriteDevice, Stderr, Stdin, Stdout};
use stak_file::{FileError, LibcFileSystem};
use stak_libc::Mmap;
use stak_process_context::LibcProcessContext;
use stak_r7rs::{SmallError, SmallPrimitiveSet};
use stak_time::LibcClock;
use stak_vm::{Value, Vm};

#[global_allocator]
static GLOBAL_ALLOCATOR: GlobalDlmalloc = GlobalDlmalloc;

#[cfg(not(test))]
#[panic_handler]
fn panic(_info: &core::panic::PanicInfo) -> ! {
    exit(1)
}

#[cfg(not(test))]
#[unsafe(no_mangle)]
#[expect(clippy::missing_const_for_fn)]
extern "C" fn rust_eh_personality() {}

#[cfg_attr(not(test), unsafe(no_mangle))]
pub extern "C" fn main(argc: isize, argv: *const *const i8) {
    // SAFETY: `argc` and `argv` are provided by operating systems.
    let options = unsafe { Options::parse_c(argc, argv) };

    if options.version() {
        let _ = writeln!(
            Stdout::new(),
            "{} {}",
            env!("CARGO_PKG_NAME"),
            env!("CARGO_PKG_VERSION")
        );

        exit(0);
    }

    let Some(index) = options.file_index() else {
        let _ = writeln!(Stderr::new(), "Error: bytecode file missing");

        exit(1);
    };

    exit(
        // SAFETY: `argc` and `argv` are provided by operating systems, the
        // index of the bytecode file is in their range, and environment
        // variables are never modified.
        match unsafe {
            run(
                options.heap_size().unwrap_or(DEFAULT_HEAP_SIZE),
                argc - index as isize,
                argv.add(index),
            )
        } {
            Ok(()) => 0,
            Err(SmallError::Halt(code)) => code.into(),
            Err(error) => {
                let _ = writeln!(Stderr::new(), "Error: {error}");

                1
            }
        },
    );
}

/// Runs a bytecode file on a virtual machine.
///
/// # Safety
///
/// The `argc` and `argv` arguments should be the ones passed down as
/// arguments to the `main` function in C without the ones before a path of a
/// bytecode file. Environment variables should not be modified while the
/// virtual machine runs.
unsafe fn run(heap_size: usize, argc: isize, argv: *const *const i8) -> Result<(), SmallError> {
    // Leak a heap to keep deallocation out of a binary.
    Vm::new(
        // SAFETY: `Value`s are scalar values.
        unsafe { Box::leak(Box::<[Value]>::new_uninit_slice(heap_size)).assume_init_mut() },
        SmallPrimitiveSet::new(
            ReadWriteDevice::new(Stdin::new(), Stdout::new(), Stderr::new()),
            LibcFileSystem::new(),
            // SAFETY: `argc` and `argv` are provided by operating systems, and
            // environment variables are never modified.
            unsafe { LibcProcessContext::new(argc, argv) },
            LibcClock::new(),
        ),
    )?
    .run(
        // SAFETY: `argv` starts with a path of a bytecode file.
        Mmap::new(unsafe { CStr::from_ptr(*argv as _) })
            .map_err(|_| SmallError::File(FileError::Open.into()))?
            .iter()
            .copied(),
    )
}
