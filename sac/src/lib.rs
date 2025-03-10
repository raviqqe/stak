//! Utilities to build executable binaries from bytecode files.

#![cfg_attr(all(doc, not(doctest)), feature(doc_auto_cfg))]
#![no_std]

#[cfg(feature = "std")]
#[doc(hidden)]
pub extern crate std;

#[doc(hidden)]
pub mod __private {
    #[cfg(feature = "std")]
    pub use clap;
    #[cfg(feature = "libc")]
    pub use libc;
    #[cfg(feature = "std")]
    pub use main_error;
    pub use stak_configuration;
    pub use stak_device;
    pub use stak_file;
    pub use stak_macro;
    pub use stak_process_context;
    pub use stak_r7rs;
    pub use stak_time;
    #[cfg(feature = "libc")]
    pub use stak_util;
    pub use stak_vm;
    #[cfg(feature = "std")]
    pub use std;
}

/// Defines a `main` function that runs a given source file.
///
/// The R7RS standard libraries are based on [the `std` crate](https://doc.rust-lang.org/std/).
///
/// The given source file is compiled into bytecodes and bundled into a
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
        use $crate::__private::{
            clap::{self, Parser},
            main_error::MainError,
            stak_device::StdioDevice,
            stak_file::OsFileSystem,
            stak_macro::include_r7rs,
            stak_process_context::OsProcessContext,
            stak_r7rs::SmallPrimitiveSet,
            stak_time::OsClock,
            stak_vm::Vm,
        };

        static _SOURCE: &str = include_str!($path);

        #[derive(clap::Parser)]
        #[command(disable_help_flag = true, ignore_errors = true, version)]
        struct Arguments {
            #[arg()]
            arguments: Vec<String>,
            #[arg(short = 's', long, default_value_t = $heap_size)]
            heap_size: usize,
        }

        fn main() -> Result<(), MainError> {
            let arguments = Arguments::parse();

            let mut heap = vec![Default::default(); arguments.heap_size];
            let mut vm = Vm::new(
                &mut heap,
                SmallPrimitiveSet::new(
                    StdioDevice::new(),
                    OsFileSystem::new(),
                    OsProcessContext::new(),
                    OsClock::new(),
                ),
            )?;

            vm.initialize(include_r7rs!($path).iter().copied())?;

            Ok(vm.run()?)
        }
    };
}

/// Defines a `main` function that runs a given source file.
///
/// The R7RS standard libraries are based on [the `libc` crate](https://docs.rs/libc).
///
/// The given source file is compiled into bytecodes and bundled into a
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
            libc::exit,
            stak_device::libc::{ReadWriteDevice, Stderr, Stdin, Stdout},
            stak_file::LibcFileSystem,
            stak_macro::include_r7rs,
            stak_process_context::LibcProcessContext,
            stak_r7rs::SmallPrimitiveSet,
            stak_time::LibcClock,
            stak_util::Heap,
            stak_vm::Vm,
        };

        static _SOURCE: &str = include_str!($path);

        #[cfg(not(test))]
        #[panic_handler]
        fn panic(_info: &core::panic::PanicInfo) -> ! {
            unsafe { exit(1) }
        }

        #[cfg_attr(not(test), unsafe(no_mangle))]
        unsafe extern "C" fn main(argc: isize, argv: *const *const i8) -> isize {
            let mut heap = Heap::new($heap_size, Default::default);
            let mut vm = Vm::new(
                heap.as_slice_mut(),
                SmallPrimitiveSet::new(
                    ReadWriteDevice::new(Stdin::new(), Stdout::new(), Stderr::new()),
                    LibcFileSystem::new(),
                    LibcProcessContext::new(argc, argv),
                    LibcClock::new(),
                ),
            )
            .unwrap();

            vm.initialize(include_r7rs!($path).iter().copied()).unwrap();
            vm.run().unwrap();

            0
        }
    };
}
