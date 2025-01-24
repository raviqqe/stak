use crate::{primitive_set::EnginePrimitiveSet, EngineError};
use any_fn::AnyFn;
use stak_module::Module;
use stak_vm::{Error, Value, Vm};

const DEFAULT_VALUE_COUNT: usize = 1 << 10;

/// A scripting engine.
pub struct Engine<'a, 'b, const N: usize = DEFAULT_VALUE_COUNT> {
    vm: Vm<'a, EnginePrimitiveSet<'a, 'b, N>>,
}

impl<'a, 'b, const N: usize> Engine<'a, 'b, N> {
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
}
