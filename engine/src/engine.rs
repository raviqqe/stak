use any_fn::AnyFn;
use stak_module::Module;
use stak_vm::{Error, Value, Vm};

use crate::primitive_set::ScriptPrimitiveSet;

/// A scripting engine.
pub struct Engine<'a, const N: usize> {
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
    pub fn run<'m>(&mut self, module: &'m impl Module<'m>) -> Result<(), Error> {
        self.vm.initialize(module.bytecode().into_iter().copied())
    }
}
