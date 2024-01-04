#[doc(hidden)]
pub mod __private {
    pub extern crate device;
    pub extern crate primitive;
    pub extern crate std;
    pub extern crate vm;
}

#[macro_export]
macro_rules! main {
    ($path:expr) => {
        use $crate::__private::{
            device::StdioDevice,
            primitive::SmallPrimitiveSet,
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

            vm.initialize(include_bytes!($path).iter().copied())?;

            Ok(vm.run()?)
        }
    };
}
