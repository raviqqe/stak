use super::Primitive;
use crate::Time;
use stak_vm::{Cons, Error, Memory, Number, PrimitiveSet};

/// A primitive set for time.
pub struct TimePrimitiveSet<T: Time> {
    time: T,
}

impl<T: Time> TimePrimitiveSet<T> {
    /// Creates a primitive set.
    pub const fn new(time: T) -> Self {
        Self { time }
    }

    fn build_string(memory: &mut Memory, string: &str) -> Result<Cons, Error> {
        let mut list = memory.null();

        for character in string.chars().rev() {
            list = memory.cons(Number::from_i64(character as _).into(), list)?;
        }

        Ok(list)
    }
}

impl<T: Time> PrimitiveSet for TimePrimitiveSet<T> {
    type Error = Error;

    fn operate(&mut self, memory: &mut Memory, primitive: usize) -> Result<(), Self::Error> {
        match primitive {
            Primitive::COMMAND_LINE => {
                memory.set_register(memory.null());

                for argument in self.time.command_line_rev() {
                    let string = Self::build_string(memory, argument)?;
                    let list = memory.cons(string.into(), memory.register())?;
                    memory.set_register(list);
                }

                memory.push(memory.register().into())?;
            }
            Primitive::ENVIRONMENT_VARIABLES => {
                memory.set_register(memory.null());

                for (key, value) in self.time.environment_variables() {
                    let pair = memory.allocate(memory.null().into(), memory.null().into())?;
                    let list = memory.cons(pair.into(), memory.register())?;
                    memory.set_register(list);

                    let string = Self::build_string(memory, key)?;
                    memory.set_car_value(memory.car(memory.register()), string.into());

                    let string = Self::build_string(memory, value)?;
                    memory.set_cdr_value(memory.car(memory.register()), string.into());
                }

                memory.push(memory.register().into())?;
            }
            _ => return Err(Error::IllegalPrimitive),
        }

        Ok(())
    }
}
