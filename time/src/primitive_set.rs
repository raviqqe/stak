mod error;
mod primitive;

pub use self::{error::PrimitiveError, primitive::Primitive};
use crate::Clock;
use stak_vm::{Error, Memory, Number, PrimitiveSet};
use winter_maybe_async::maybe_async;

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
    type Error = PrimitiveError;

    #[maybe_async]
    fn operate(&mut self, memory: &mut Memory<'_>, primitive: usize) -> Result<(), Self::Error> {
        match primitive {
            Primitive::CURRENT_JIFFY => {
                memory.push(
                    Number::from_i64(
                        self.clock
                            .current_jiffy()
                            .map_err(|_| PrimitiveError::CurrentJiffy)?
                            as _,
                    )
                    .into(),
                )?;
            }
            _ => return Err(Error::IllegalPrimitive.into()),
        }

        Ok(())
    }
}
