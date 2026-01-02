use stak_vm::{Error, Heap, Memory, PrimitiveSet};
use winter_maybe_async::maybe_async;

/// An arithmetic primitive.
pub enum ArithmeticPrimitive {
    /// A `quotient` procedure.
    Quotient,
}

impl ArithmeticPrimitive {
    const QUOTIENT: usize = Self::Quotient as _;
}

/// An arithmetic primitive set.
#[derive(Debug, Default)]
pub struct ArithmeticPrimitiveSet {}

impl ArithmeticPrimitiveSet {
    /// Creates a primitive set.
    pub fn new() -> Self {
        Self::default()
    }
}

impl<H: Heap> PrimitiveSet<H> for ArithmeticPrimitiveSet {
    type Error = Error;

    #[maybe_async]
    fn operate(&mut self, memory: &mut Memory<H>, primitive: usize) -> Result<(), Self::Error> {
        match primitive {
            ArithmeticPrimitive::QUOTIENT => {
                let [x, y] = memory.pop_many()?;
                let x = x.assume_number();
                let y = y.assume_number();

                memory.push(((x - x % y) / y).into())?;
            }
            _ => return Err(Error::IllegalPrimitive),
        }

        Ok(())
    }
}
