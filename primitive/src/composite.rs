mod error;

use stak_vm::{PrimitiveSet, Vm};

/// A composite primitive set.
pub struct CompositePrimitiveSet<'a, const N: usize> {
    primitive_sets: [&'a dyn PrimitiveSet; N],
}

impl<'a, const N: usize> CompositePrimitiveSet<'a, N> {
    /// Creates a primitive set.
    pub fn new(primitive_sets: [&'a dyn PrimitiveSet; N]) -> Self {
        Self { primitive_sets }
    }
}

impl<'a, N> PrimitiveSet for CompositePrimitiveSet<'a, N> {
    type Error = CompositeError;

    fn operate(vm: &mut Vm<Self>, primitive: u8) -> Result<(), Error> {
        todo!()
    }
}
