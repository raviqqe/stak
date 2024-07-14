//! Utilities to build executable binaries from bytecode files.

#[doc(hidden)]
pub mod __private {
    #[cfg(feature = "std")]
    pub use clap;
    #[cfg(feature = "std")]
    pub use main_error;
    pub use stak_configuration;
    pub use stak_device;
    pub use stak_file;
    pub use stak_macro;
    pub use stak_primitive;
    pub use stak_process_context;
    pub use stak_vm;
    #[cfg(feature = "std")]
    pub use std;
}

/// Defines a `main` function that executes a source file at a given path.
///
/// The given source file is compiled into bytecodes and bundled into a
/// resulting binary.
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
            stak_primitive::SmallPrimitiveSet,
            stak_process_context::OsProcessContext,
            stak_vm::Vm,
        };

        #[derive(clap::Parser)]
        #[command(about, version)]
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
                ),
            )?;

            vm.initialize(include_r7rs!($path).iter().copied())?;

            Ok(vm.run()?)
        }
    };
}

/// Defines a `main` function that executes a source file at a given path.
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
            stak_device::libc::{ReadWriteDevice, Stderr, Stdin, Stdout},
            stak_file::LibcFileSystem,
            stak_macro::include_r7rs,
            stak_primitive::SmallPrimitiveSet,
            stak_process_context::VoidProcessContext,
            stak_vm::Vm,
        };

        #[cfg_attr(not(test), no_mangle)]
        unsafe extern "C" fn main(argc: isize, argv: *const *const i8) -> isize {
            let mut heap = [Default::default(); $heap_size];
            let mut vm = Vm::new(
                &mut heap,
                SmallPrimitiveSet::new(
                    ReadWriteDevice::new(Stdin::new(), Stdout::new(), Stderr::new()),
                    LibcFileSystem::new(),
                    VoidProcessContext::new(),
                ),
            )
            .unwrap();

            vm.initialize(include_r7rs!($path).iter().copied()).unwrap();
            vm.run().unwrap();

            0
        }
    };
}
