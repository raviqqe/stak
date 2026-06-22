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

                memory.push((x - x.remainder(y)?).divide(y)?.into())?;
            }
            _ => return Err(Error::IllegalPrimitive),
        }

        Ok(())
    }
}

#[cfg(all(test, not(feature = "async")))]
mod tests {
    use super::*;
    use stak_vm::{Number, Value};

    const HEAP_SIZE: usize = 1 << 8;

    fn quotient(x: i64, y: i64) -> Result<i64, Error> {
        let mut memory = Memory::new([Value::default(); HEAP_SIZE]).unwrap();
        memory.set_stack(memory.null().unwrap());
        memory.push(Number::from_i64(x).into()).unwrap();
        memory.push(Number::from_i64(y).into()).unwrap();

        ArithmeticPrimitiveSet::new().operate(&mut memory, ArithmeticPrimitive::QUOTIENT)?;

        Ok(memory.pop().unwrap().assume_number().to_i64())
    }

    #[test]
    fn calculate_quotient() {
        assert_eq!(quotient(7, 2), Ok(3));
        assert_eq!(quotient(6, 3), Ok(2));
        assert_eq!(quotient(-7, 2), Ok(-3));
    }
}
