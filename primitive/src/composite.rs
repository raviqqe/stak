mod error;

use self::error::{CompositeError, IllegalPrimitiveError};
use core::fmt::{Debug, Display};
use stak_vm::{Memory, PrimitiveSet};

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

    fn operate(&mut self, vm: &mut Memory, primitive: u8) -> Result<(), Self::Error> {
        let Err(error) = self.first.operate(vm, primitive) else {
            return Ok(());
        };

        if !error.is_illegal_primitive() {
            return Err(CompositeError::Primary(error));
        }

        self.second
            .operate(vm, primitive)
            .map_err(CompositeError::Secondary)?;

        Ok(())
    }
}
