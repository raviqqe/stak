//! Utilities to build executable binaries from bytecode files.

#![cfg_attr(all(doc, not(doctest)), feature(doc_cfg))]
#![no_std]

#[cfg(feature = "libc")]
#[doc(hidden)]
pub extern crate alloc;

mod options;

#[doc(hidden)]
pub mod __private {
    pub use crate::options::Options;
    #[cfg(feature = "libc")]
    pub use alloc;
    #[cfg(feature = "libc")]
    pub use dlmalloc;
    #[cfg(feature = "std")]
    pub use main_error;
    #[cfg(feature = "libc")]
    pub use origin;
    pub use stak_configuration;
    pub use stak_device;
    pub use stak_file;
    pub use stak_macro;
    pub use stak_process_context;
    pub use stak_r7rs;
    pub use stak_time;
    pub use stak_vm;
}

/// Defines a `main` function that runs a given source file.
///
/// The R7RS standard libraries are based on [the `std` crate](https://doc.rust-lang.org/std/).
///
/// The given source file is compiled into bytecode and bundled into a
/// resulting binary.
///
/// # Examples
///
/// ```rust
/// use stak::sac::main;
///
/// main!("main.scm");
/// ```
#[cfg(feature = "std")]
#[macro_export]
macro_rules! main {
    ($path:expr) => {
        $crate::main!(
            $path,
            $crate::__private::stak_configuration::DEFAULT_HEAP_SIZE
        );
    };
    ($path:expr, $heap_size:expr) => {
        use std::{
            env::args,
            io::{Write, stdout},
            process::ExitCode,
        };
        use $crate::__private::{
            Options,
            main_error::MainError,
            stak_device::StdioDevice,
            stak_file::OsFileSystem,
            stak_macro::include_r7rs,
            stak_process_context::OsProcessContext,
            stak_r7rs::{SmallError, SmallPrimitiveSet},
            stak_time::OsClock,
            stak_vm::Vm,
        };

        fn main() -> Result<ExitCode, MainError> {
            let options = Options::parse(&args().collect::<Vec<_>>());

            if options.version() {
                writeln!(
                    stdout(),
                    "{} {}",
                    env!("CARGO_PKG_NAME"),
                    env!("CARGO_PKG_VERSION")
                )?;

                return Ok(ExitCode::SUCCESS);
            }

            match Vm::new(
                vec![Default::default(); options.heap_size().unwrap_or($heap_size)],
                SmallPrimitiveSet::new(
                    StdioDevice::new(),
                    OsFileSystem::new(),
                    OsProcessContext::new(),
                    OsClock::new(),
                ),
            )?
            .run(include_r7rs!($path).iter().copied())
            {
                Ok(()) => Ok(ExitCode::SUCCESS),
                Err(SmallError::Halt(code)) => Ok(code.into()),
                Err(error) => Err(error.into()),
            }
        }
    };
}

/// Defines a `main` function that runs a given source file.
///
/// The R7RS standard libraries are based on [the `libc` crate](https://docs.rs/libc).
///
/// The given source file is compiled into bytecode and bundled into a
/// resulting binary.
#[cfg(feature = "libc")]
#[macro_export]
macro_rules! libc_main {
    ($path:expr) => {
        $crate::libc_main!(
            $path,
            $crate::__private::stak_configuration::DEFAULT_HEAP_SIZE
        );
    };
    ($path:expr, $heap_size:expr) => {
        use core::{ffi::CStr, fmt::Write, slice};
        use $crate::__private::{
            Options,
            alloc::{boxed::Box, vec::Vec},
            dlmalloc::GlobalDlmalloc,
            origin::program::exit,
            stak_device::libc::{ReadWriteDevice, Stderr, Stdin, Stdout},
            stak_file::LibcFileSystem,
            stak_macro::include_r7rs,
            stak_process_context::LibcProcessContext,
            stak_r7rs::{SmallError, SmallPrimitiveSet},
            stak_time::LibcClock,
            stak_vm::{Value, Vm},
        };

        #[global_allocator]
        static GLOBAL_ALLOCATOR: GlobalDlmalloc = GlobalDlmalloc;

        #[cfg(not(test))]
        #[panic_handler]
        fn panic(_info: &core::panic::PanicInfo) -> ! {
            exit(1)
        }

        #[cfg_attr(not(test), unsafe(no_mangle))]
        extern "C" fn main(argc: isize, argv: *const *const i8) {
            // Leak arguments to keep deallocation out of a binary.
            let options = Options::parse(
                // SAFETY: Operating systems guarantee `argv` to have the length of
                // `argc`.
                unsafe { slice::from_raw_parts(argv, argc as _) }
                    .iter()
                    .map(|&argument| {
                        // SAFETY: Operating systems guarantee elements in `argv` to be
                        // C strings.
                        unsafe { CStr::from_ptr(argument as _) }
                            .to_str()
                            .unwrap_or_default()
                    })
                    .collect::<Vec<_>>()
                    .leak(),
            );

            if options.version() {
                let _ = writeln!(
                    Stdout::new(),
                    "{} {}",
                    env!("CARGO_PKG_NAME"),
                    env!("CARGO_PKG_VERSION")
                );

                exit(0);
            }

            let mut heap =
                Box::<[Value]>::new_uninit_slice(options.heap_size().unwrap_or($heap_size));

            exit(
                match Vm::new(
                    // SAFETY: `Value`s are scalar values.
                    unsafe { heap.assume_init_mut() },
                    SmallPrimitiveSet::new(
                        ReadWriteDevice::new(Stdin::new(), Stdout::new(), Stderr::new()),
                        LibcFileSystem::new(),
                        // SAFETY: `argc` and `argv` are provided by operating systems,
                        // and environment variables are never modified.
                        unsafe { LibcProcessContext::new(argc, argv) },
                        LibcClock::new(),
                    ),
                )
                .map_err(SmallError::from)
                .and_then(|mut vm| vm.run(include_r7rs!($path).iter().copied()))
                {
                    Ok(()) => 0,
                    Err(SmallError::Halt(code)) => code.into(),
                    Err(error) => {
                        let _ = writeln!(Stderr::new(), "Error: {error}");

                        1
                    }
                },
            );
        }
    };
}
