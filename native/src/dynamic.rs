mod function;

pub use self::function::DynamicFunction;
use alloc::boxed::Box;
use core::any::Any;
use heapless::Vec;
use stak_vm::{Error, Memory, Number, PrimitiveSet, Type};

const MAXIMUM_ARGUMENT_COUNT: usize = 32;

/// A dynamic primitive set equipped with native functions in Rust.
pub struct DynamicPrimitiveSet<'a, const N: usize> {
    functions: &'a mut [&'a mut dyn DynamicFunction],
    objects: [Option<Box<dyn Any>>; N],
}

impl<'a, const N: usize> DynamicPrimitiveSet<'a, N> {
    /// Creates a primitive set.
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
        let function: &mut dyn DynamicFunction = *self
            .functions
            .get_mut(primitive)
            .ok_or(Error::IllegalPrimitive)?;

        let (value, index) = {
            let mut arguments = Vec::<_, MAXIMUM_ARGUMENT_COUNT>::new();

            for _ in 0..function.parameter_count() {
                let value = memory.pop();
                let value = self.objects
                    [memory.car(value.assume_cons()).assume_number().to_i64() as usize]
                    .as_ref()
                    .ok_or(Error::OutOfMemory)?;
                // TODO Handle an error.
                arguments.push(value).unwrap();
            }

            let value = function.call(arguments.as_slice());

            (
                value,
                self.objects
                    .iter()
                    .position(|object| matches!(object, None))
                    .ok_or(Error::OutOfMemory)?,
            )
        };

        self.objects[index] = Some(value);

        let cons = memory.cons(
            Number::from_i64(index as _).into(),
            memory.null().set_tag(Type::Foreign as _),
        )?;
        memory.push(cons.into())?;

        Ok(())
    }
}
