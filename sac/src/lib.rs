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
        use $crate::__private::{
            alloc::boxed::Box,
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
            let mut heap = Box::<[Value; $heap_size]>::new_uninit();

            exit(
                match Vm::new(
                    unsafe { heap.assume_init_mut() },
                    SmallPrimitiveSet::new(
                        ReadWriteDevice::new(Stdin::new(), Stdout::new(), Stderr::new()),
                        LibcFileSystem::new(),
                        unsafe { LibcProcessContext::new(argc, argv) },
                        LibcClock::new(),
                    ),
                )
                .unwrap()
                .run(include_r7rs!($path).iter().copied())
                {
                    Ok(()) => 0,
                    Err(SmallError::Halt(code)) => code.into(),
                    Err(_) => 1,
                },
            );
        }
    };
}
