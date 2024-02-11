//! Utilities to build executable binaries from bytecode files.

#[doc(hidden)]
pub mod __private {
    pub use clap;
    pub use main_error;
    pub use stak_device;
    pub use stak_macro;
    pub use stak_primitive;
    pub use stak_vm;
    pub use std;
}

/// Defines a `main` function that executes a bytecode file at a given path.
///
/// The given bytecode file is bundled into a resulting binary.
#[macro_export]
macro_rules! main {
    ($path:expr) => {
        use $crate::__private::{
            clap::{self, Parser},
            main_error::MainError,
            stak_device::StdioDevice,
            stak_macro::include_r7rs,
            stak_primitive::SmallPrimitiveSet,
            stak_vm::Vm,
            std::{env, error::Error},
        };

        const DEFAULT_HEAP_SIZE: usize = 1 << 20;

        #[derive(clap::Parser)]
        #[command(about, version)]
        struct Arguments {
            #[arg(short = 's', long, default_value_t = DEFAULT_HEAP_SIZE)]
            heap_size: usize,
        }

        fn main() -> Result<(), MainError> {
            let arguments = Arguments::parse();

            let mut heap = vec![Default::default(); arguments.heap_size];
            let mut vm = Vm::new(&mut heap, SmallPrimitiveSet::new(StdioDevice::new()))?;

            vm.initialize(include_r7rs!($path).iter().copied())?;

            Ok(vm.run()?)
        }
    };
}
