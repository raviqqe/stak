mod primitive;

pub use self::primitive::Primitive;
use crate::Clock;
use stak_vm::{Error, Memory, PrimitiveSet};

/// A primitive set for time.
pub struct TimePrimitiveSet<T: Clock> {
    clock: T,
}

impl<T: Clock> TimePrimitiveSet<T> {
    /// Creates a primitive set.
    pub const fn new(clock: T) -> Self {
        Self { clock }
    }
}

impl<T: Clock> PrimitiveSet for TimePrimitiveSet<T> {
    type Error = Error;

    fn operate(&mut self, memory: &mut Memory, primitive: usize) -> Result<(), Self::Error> {
        match primitive {
            Primitive::CURRENT_JIFFY => {
                memory.push(
                    self.clock
                        .current_jiffy()
                        .map(From::from)
                        .unwrap_or(memory.boolean(false).into()),
                )?;
            }
            _ => return Err(Error::IllegalPrimitive),
        }

        Ok(())
    }
}
