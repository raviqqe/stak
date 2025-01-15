mod function;

pub use self::function::DynamicFunction;
use core::any::Any;
use heapless::Vec;
use stak_vm::{Error, Memory, PrimitiveSet};

const MAXIMUM_ARGUMENT_COUNT: usize = 32;

/// A dynamic primitive set equipped with native functions in Rust.
pub struct DynamicPrimitiveSet<'a, const N: usize> {
    functions: &'a mut [&'a mut dyn DynamicFunction],
    objects: [Option<&'a mut dyn Any>; N],
}

impl<'a, const N: usize> DynamicPrimitiveSet<'a, N> {
    pub fn new(functions: &'a mut [&'a mut dyn DynamicFunction]) -> Self {
        Self {
            functions,
            objects: [const { None }; N],
        }
    }
}

impl<'a, const N: usize> PrimitiveSet for DynamicPrimitiveSet<'a, N> {
    type Error = Error;

    fn operate(&mut self, memory: &mut Memory, primitive: usize) -> Result<(), Self::Error> {
        let function = self
            .functions
            .get(primitive)
            .ok_or(Error::IllegalPrimitive)?;
        let mut arguments = Vec::<_, MAXIMUM_ARGUMENT_COUNT>::new();

        for _ in 0..function.parameter_count() {
            arguments.push(
                self.objects[memory
                    .car(memory.pop().assume_cons())
                    .assume_number()
                    .to_i64() as _],
            );
        }

        let value = function.call(&arguments);
        memory.push(value);

        Ok(())
    }
}
