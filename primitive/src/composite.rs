mod error;

use self::error::{CompositeError, IllegalPrimitiveError};
use core::fmt::{Debug, Display};
use stak_vm::{PrimitiveSet, Vm};

/// A composite primitive set.
pub struct CompositePrimitiveSet<P: PrimitiveSet, Q: PrimitiveSet> {
    first: P,
    second: Q,
}

impl<P: PrimitiveSet, Q: PrimitiveSet> CompositePrimitiveSet<P, Q> {
    /// Creates a primitive set.
    pub fn new(first: P, second: Q) -> Self {
        Self { first, second }
    }
}

impl<P: PrimitiveSet, Q: PrimitiveSet> PrimitiveSet for CompositePrimitiveSet<P, Q>
where
    P::Error: IllegalPrimitiveError + Debug + Display,
    Q::Error: IllegalPrimitiveError + Debug + Display,
{
    type Error = CompositeError<P::Error, Q::Error>;

    fn operate(vm: &mut Vm<Self>, primitive: u8) -> Result<(), Self::Error> {
        let Err(error) = vm.primitive_set().first.operate(vm, primitive) else {
            return Ok(());
        };

        if !error.is_illegal_primitive() {
            return Err(error);
        }

        vm.primitive_set()
            .second
            .operate(vm, primitive)
            .map_err(CompositeError::First)
    }
}
