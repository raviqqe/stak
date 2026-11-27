//! Utilities to build executable binaries from bytecode files.

#![cfg_attr(all(doc, not(doctest)), feature(doc_cfg))]
#![no_std]

#[cfg(any(feature = "libc", feature = "std"))]
extern crate alloc;

mod options;
mod run;

#[doc(hidden)]
pub mod __private {
    pub use crate::options::Options;
    #[cfg(feature = "std")]
    pub use crate::run::run;
    #[cfg(feature = "libc")]
    pub use crate::run::run_libc;
    #[cfg(feature = "libc")]
    pub use dlmalloc;
    #[cfg(feature = "std")]
    pub use main_error;
    #[cfg(feature = "libc")]
    pub use origin;
    pub use stak_configuration;
    pub use stak_device;
    pub use stak_macro;
    pub use stak_r7rs;
}

/// Defines a `main` function that runs a given source file.
///
/// With the `std` feature, the R7RS standard libraries are based on [the `std` crate](https://doc.rust-lang.org/std/).
/// Otherwise, they are based on [the `libc` crate](https://docs.rs/libc).
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
#[cfg(any(feature = "libc", feature = "std"))]
#[macro_export]
macro_rules! main {
    ($path:expr) => {
        $crate::main!(
            $path,
            $crate::__private::stak_configuration::DEFAULT_HEAP_SIZE
        );
    };
    ($path:expr, $heap_size:expr) => {
        $crate::__main!($path, $heap_size);
    };
}

cfg_select! {
    feature = "std" => {
        #[doc(hidden)]
        #[macro_export]
        macro_rules! __main {
            ($path:expr, $heap_size:expr) => {
                use std::{
                    env::args,
                    io::{Write, stdout},
                    process::ExitCode,
                };
                use $crate::__private::{
                    Options, main_error::MainError, run, stak_macro::include_r7rs,
                    stak_r7rs::SmallError,
                };

                fn main() -> Result<ExitCode, MainError> {
                    let options = Options::parse(args())?;

                    if options.version() {
                        writeln!(
                            stdout(),
                            "{} {}",
                            env!("CARGO_PKG_NAME"),
                            env!("CARGO_PKG_VERSION")
                        )?;

                        return Ok(ExitCode::SUCCESS);
                    }

                    match run(
                        include_r7rs!($path),
                        options.heap_size().unwrap_or($heap_size),
                        options.argument_index(),
                    ) {
                        Ok(()) => Ok(ExitCode::SUCCESS),
                        Err(SmallError::Halt(code)) => Ok(code.into()),
                        Err(error) => Err(error.into()),
                    }
                }
            };
        }
    }
    feature = "libc" => {
        // The `libc` crate links the C library on musl only as a dependency of
        // `std`. The dynamic kind places it after the compiler built-ins, which
        // need it.
        #[cfg(target_env = "musl")]
        #[link(name = "c", kind = "dylib")]
        unsafe extern "C" {}

        #[doc(hidden)]
        #[macro_export]
        macro_rules! __main {
            ($path:expr, $heap_size:expr) => {
                use core::fmt::Write;
                use $crate::__private::{
                    Options,
                    dlmalloc::GlobalDlmalloc,
                    origin::program::exit,
                    run_libc,
                    stak_device::libc::{Stderr, Stdout},
                    stak_macro::include_r7rs,
                    stak_r7rs::SmallError,
                };

                #[global_allocator]
                static GLOBAL_ALLOCATOR: GlobalDlmalloc = GlobalDlmalloc;

                #[cfg(not(test))]
                #[panic_handler]
                fn panic(_info: &core::panic::PanicInfo) -> ! {
                    exit(1)
                }

                #[cfg(not(test))]
                #[unsafe(no_mangle)]
                extern "C" fn rust_eh_personality() {}

                #[cfg_attr(not(test), unsafe(no_mangle))]
                extern "C" fn main(argc: isize, argv: *const *const i8) {
                    // SAFETY: `argc` and `argv` are provided by operating systems.
                    let options = unsafe { Options::parse_c(argc, argv) }.unwrap_or_else(|error| {
                        let _ = writeln!(Stderr::new(), "Error: {error}");

                        exit(1)
                    });

                    if options.version() {
                        let _ = writeln!(
                            Stdout::new(),
                            "{} {}",
                            env!("CARGO_PKG_NAME"),
                            env!("CARGO_PKG_VERSION")
                        );

                        exit(0);
                    }

                    exit(
                        // SAFETY: `argc` and `argv` are provided by operating systems, and
                        // environment variables are never modified.
                        match unsafe {
                            run_libc(
                                include_r7rs!($path),
                                options.heap_size().unwrap_or($heap_size),
                                options.argument_index(),
                                argc,
                                argv,
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
            };
        }
    }
    _ => {}
}
