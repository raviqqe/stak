use crate::{primitive_set::ScriptPrimitiveSet, ScriptError};
use any_fn::AnyFn;
use stak_module::Module;
use stak_vm::{Error, Value, Vm};

const DEFAULT_VALUE_COUNT: usize = 1 << 10;

/// A scripting engine.
pub struct Engine<'a, const N: usize = DEFAULT_VALUE_COUNT> {
    vm: Vm<'a, ScriptPrimitiveSet<'a, N>>,
}

impl<'a, const N: usize> Engine<'a, N> {
    /// Creates a scripting engine.
    pub fn new(heap: &'a mut [Value], functions: &'a mut [AnyFn<'a>]) -> Result<Self, Error> {
        Ok(Self {
            vm: Vm::new(heap, ScriptPrimitiveSet::new(functions))?,
        })
    }

    /// Runs a module.
    pub fn run(&mut self, module: &'static impl Module<'static>) -> Result<(), ScriptError> {
        self.vm.initialize(module.bytecode().into_iter().copied())?;
        self.vm.run()
    }
}
