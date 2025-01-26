//! Native functions dynamically defined.

mod error;

pub use self::error::DynamicError;
use any_fn::AnyFn;
use heapless::Vec;
use stak_vm::{Error, Memory, Number, PrimitiveSet, Type};

const MAXIMUM_ARGUMENT_COUNT: usize = 16;

/// A dynamic primitive set equipped with native functions in Rust.
pub struct DynamicPrimitiveSet<'a, 'b, const N: usize> {
    functions: &'a mut [AnyFn<'b>],
    values: [Option<any_fn::Value>; N],
}

impl<'a, 'b, const N: usize> DynamicPrimitiveSet<'a, 'b, N> {
    /// Creates a primitive set.
    pub fn new(functions: &'a mut [AnyFn<'b>]) -> Self {
        Self {
            functions,
            // TODO Garbage-collect foreign values.
            values: [const { None }; N],
        }
    }
}

impl<const N: usize> PrimitiveSet for DynamicPrimitiveSet<'_, '_, N> {
    type Error = DynamicError;

    fn operate(&mut self, memory: &mut Memory, primitive: usize) -> Result<(), Self::Error> {
        let function = self
            .functions
            .get_mut(primitive)
            .ok_or(Error::IllegalPrimitive)?;

        let (value, index) = {
            let mut arguments = Vec::<_, MAXIMUM_ARGUMENT_COUNT>::new();

            for _ in 0..function.arity() {
                let value = memory.pop();
                let value = self.values
                    [memory.car(value.assume_cons()).assume_number().to_i64() as usize]
                    .as_ref()
                    .ok_or(DynamicError::ObjectIndex)?;

                arguments.push(value).map_err(|_| Error::ArgumentCount)?;
            }

            let value = function.call(arguments.as_slice())?;

            (
                value,
                self.values
                    .iter()
                    .position(Option::is_none)
                    .ok_or(Error::OutOfMemory)?,
            )
        };

        self.values[index] = Some(value);

        let cons = memory.cons(
            Number::from_i64(index as _).into(),
            memory.null().set_tag(Type::Foreign as _),
        )?;
        memory.push(cons.into())?;

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use any_fn::IntoAnyFn;

    struct Person {
        pies: usize,
        dodge: f64,
        wasted: bool,
    }

    impl Person {
        pub fn new(pies: usize, dodge: f64) -> Self {
            Self {
                pies,
                dodge,
                wasted: false,
            }
        }

        pub fn throw_pie(&mut self, other: &mut Person) {
            if self.wasted {
                return;
            }

            self.pies -= 1;

            if random::<f64>() > other.dodge {
                other.wasted = true;
            }
        }
    }

    #[test]
    fn create() {
        let mut functions = [Person::new.into_any_fn(), Person::throw_pie.into_any_fn()];
        let mut engine = DynamicPrimitiveSet::new(&mut functions)?;
    }
}
