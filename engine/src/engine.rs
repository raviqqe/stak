use crate::{primitive_set::EnginePrimitiveSet, EngineError};
use any_fn::AnyFn;
#[cfg(doc)]
use stak_dynamic::DynamicPrimitiveSet;
use stak_dynamic::SchemeValue;
use stak_module::Module;
use stak_vm::{Error, Value, Vm};

/// A scripting engine.
pub struct Engine<'a, 'b> {
    vm: Vm<'a, EnginePrimitiveSet<'a, 'b>>,
}

impl<'a, 'b> Engine<'a, 'b> {
    /// Creates a scripting engine.
    pub fn new(heap: &'a mut [Value], functions: &'a mut [AnyFn<'b>]) -> Result<Self, Error> {
        Ok(Self {
            vm: Vm::new(heap, EnginePrimitiveSet::new(functions))?,
        })
    }

    /// Runs a module.
    pub fn run(&mut self, module: &'static impl Module<'static>) -> Result<(), EngineError> {
        self.vm.initialize(module.bytecode().iter().copied())?;
        self.vm.run()
    }

    /// Registers a type compatible between Scheme and Rust.
    ///
    /// We register all types that this crate implements [`SchemeValue`] for to
    /// the engines by default.
    ///
    /// For more information, see [`DynamicPrimitiveSet::register_type`].
    pub fn register_type<T: SchemeValue + 'static>(&mut self) {
        self.vm
            .primitive_set_mut()
            .dynamic_mut()
            .register_type::<T>()
    }
}
