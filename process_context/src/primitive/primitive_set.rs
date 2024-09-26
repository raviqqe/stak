use super::Primitive;
use crate::ProcessContext;
use stak_vm::{Cons, Error, Memory, Number, PrimitiveSet};

/// A primitive set for process context.
pub struct ProcessContextPrimitiveSet<T: ProcessContext> {
    process_context: T,
}

impl<T: ProcessContext> ProcessContextPrimitiveSet<T> {
    /// Creates a primitive set.
    pub const fn new(process_context: T) -> Self {
        Self { process_context }
    }

    fn build_string(memory: &mut Memory, string: &str) -> Result<Cons, Error> {
        let mut list = memory.null();

        for character in string.chars().rev() {
            list = memory.cons(Number::from_i64(character as _).into(), list)?;
        }

        Ok(list)
    }
}

impl<T: ProcessContext> PrimitiveSet for ProcessContextPrimitiveSet<T> {
    type Error = Error;

    fn operate(&mut self, memory: &mut Memory, primitive: u8) -> Result<(), Self::Error> {
        match primitive {
            Primitive::COMMAND_LINE => {
                memory.set_register(memory.null());

                for argument in self.process_context.command_line_rev() {
                    let string = Self::build_string(memory, argument)?;
                    let list = memory.cons(string.into(), memory.register())?;
                    memory.set_register(list);
                }

                memory.push(memory.register().into())?;
            }
            Primitive::ENVIRONMENT_VARIABLES => {
                memory.set_register(memory.null());

                for (key, value) in self.process_context.environment_variables() {
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
