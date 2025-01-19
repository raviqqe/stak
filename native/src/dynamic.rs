//! Native functions dynamically defined.

mod error;
mod function;

pub use self::{
    error::DynamicError,
    function::{DynamicFunction, IntoDynamicFunction},
};
use alloc::boxed::Box;
use core::any::Any;
use core::cell::RefCell;
use heapless::Vec;
use stak_vm::{Error, Memory, Number, PrimitiveSet, Type};

const MAXIMUM_ARGUMENT_COUNT: usize = 32;

/// A dynamic primitive set equipped with native functions in Rust.
pub struct DynamicPrimitiveSet<'a, const N: usize> {
    functions: &'a mut [DynamicFunction<'a>],
    objects: [Option<RefCell<Box<dyn Any>>>; N],
}

impl<'a, const N: usize> DynamicPrimitiveSet<'a, N> {
    /// Creates a primitive set.
    pub fn new(functions: &'a mut [DynamicFunction<'a>]) -> Self {
        Self {
            functions,
            // TODO Garbage-collect foreign objects.
            objects: [const { None }; N],
        }
    }
}

impl<const N: usize> PrimitiveSet for DynamicPrimitiveSet<'_, N> {
    type Error = DynamicError;

    fn operate(&mut self, memory: &mut Memory, primitive: usize) -> Result<(), Self::Error> {
        let function: &mut DynamicFunction = self
            .functions
            .get_mut(primitive)
            .ok_or(Error::IllegalPrimitive)?;

        let (value, index) = {
            let mut arguments = Vec::<&dyn Any, MAXIMUM_ARGUMENT_COUNT>::new();

            for _ in 0..function.arity() {
                let value = memory.pop();
                // TODO Convert Scheme values into Rust values automatically?
                let value = self.objects
                    [memory.car(value.assume_cons()).assume_number().to_i64() as usize]
                    .as_ref()
                    .ok_or(DynamicError::ObjectIndex)?;

                arguments.push(value).map_err(|_| Error::ArgumentCount)?;
            }

            let value = function.call(arguments.as_slice())?;

            (
                value,
                self.objects
                    .iter()
                    .position(Option::is_none)
                    .ok_or(Error::OutOfMemory)?,
            )
        };

        self.objects[index] = Some(RefCell::new(value));

        let cons = memory.cons(
            Number::from_i64(index as _).into(),
            memory.null().set_tag(Type::Foreign as _),
        )?;
        memory.push(cons.into())?;

        Ok(())
    }
}
