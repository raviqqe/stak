use crate::{EngineError, primitive_set::EnginePrimitiveSet};
use any_fn::AnyFn;
use stak_dynamic::SchemeValue;
use stak_module::Module;
use stak_util::block_on;
use stak_vm::{Error, Heap, Vm};
use winter_maybe_async::{maybe_async, maybe_await};

/// A scripting engine.
pub struct Engine<'a, 'b, H: Heap> {
    vm: Vm<'a, EnginePrimitiveSet<'a, 'b, H>, H>,
}

impl<'a, 'b, H: Heap> Engine<'a, 'b, H> {
    /// Creates a scripting engine.
    pub fn new(
        heap: H,
        functions: &'a mut [(&'a str, AnyFn<'b>)],
    ) -> Result<Self, Error> {
        Ok(Self {
            vm: Vm::new(heap, EnginePrimitiveSet::new(functions))?,
        })
    }

    /// Runs a module synchronously.
    ///
    /// # Panics
    ///
    /// Panics if asynchronous operations occur during the run.
    pub fn run<'c>(&mut self, module: &'c impl Module<'c>) -> Result<(), EngineError> {
        block_on!(self.run_async(module))
    }

    /// Runs a module.
    #[cfg_attr(not(feature = "async"), doc(hidden))]
    #[maybe_async]
    pub fn run_async<'c>(&mut self, module: &'c impl Module<'c>) -> Result<(), EngineError> {
        self.vm.initialize(module.bytecode().iter().copied())?;
        maybe_await!(self.vm.run_async())
    }

    /// Registers a type compatible between Scheme and Rust.
    ///
    /// We register all types that this crate implements [`SchemeValue`] for to
    /// the engines by default.
    ///
    /// For more information, see
    /// [`DynamicPrimitiveSet`][stak_dynamic::DynamicPrimitiveSet].
    pub fn register_type<T: SchemeValue<H> + 'static>(&mut self) {
        self.vm
            .primitive_set_mut()
            .dynamic_mut()
            .register_type::<T>()
    }
}
