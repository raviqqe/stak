//! Utilities to build executable binaries from bytecode files.

#[doc(hidden)]
pub mod __private {
    pub extern crate stak_device;
    pub extern crate stak_macro;
    pub extern crate stak_primitive;
    pub extern crate stak_vm;
    pub extern crate std;
}

/// Defines a `main` function that executes a bytecode file at a given path.
///
/// The given bytecode file is bundled into a resulting binary.
#[macro_export]
macro_rules! main {
    ($path:expr) => {
        use $crate::__private::{
            stak_device::StdioDevice,
            stak_macro::include_r7rs,
            stak_primitive::SmallPrimitiveSet,
            stak_vm::Vm,
            std::{env, error::Error},
        };

        const DEFAULT_HEAP_SIZE: usize = 1 << 20;

        fn main() -> Result<(), Box<dyn Error>> {
            let size = env::var("STAK_HEAP_SIZE")
                .ok()
                .map(|string| string.parse())
                .transpose()?
                .unwrap_or(DEFAULT_HEAP_SIZE);
            let mut heap = vec![Default::default(); size];
            let mut vm = Vm::new(&mut heap, SmallPrimitiveSet::new(StdioDevice::new()))?;

            vm.initialize(include_r7rs!($path).iter().copied())?;

            Ok(vm.run()?)
        }
    };
}
