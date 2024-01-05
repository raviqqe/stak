//! Utilities to build executable binaries from bytecode files.

#[doc(hidden)]
pub mod __private {
    pub extern crate device;
    pub extern crate primitive;
    pub extern crate stak_macro;
    pub extern crate std;
    pub extern crate vm;
}

/// Defines a `main` function that executes a bytecode file at a given path.
///
/// The given bytecode file is bundled into a resulting binary.
#[macro_export]
macro_rules! main {
    ($path:expr) => {
        use $crate::__private::{
            device::StdioDevice,
            primitive::SmallPrimitiveSet,
            stak_macro::r7rs,
            std::{env, error::Error, process::exit},
            vm::Vm,
        };

        const DEFAULT_HEAP_SIZE: usize = 1 << 21;

        fn main() -> Result<(), Box<dyn Error>> {
            let size = env::var("STAK_HEAP_SIZE")
                .ok()
                .map(|string| string.parse())
                .transpose()?
                .unwrap_or(DEFAULT_HEAP_SIZE);
            let mut heap = vec![Default::default(); size];
            let mut vm = Vm::new(&mut heap, SmallPrimitiveSet::new(StdioDevice::new()))?;

            vm.initialize(r7rs!($path).iter().copied())?;

            Ok(vm.run()?)
        }
    };
}
