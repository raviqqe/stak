//! Native functions dynamically defined.

mod error;

pub use self::error::DynamicError;
use any_fn::AnyFn;
use core::any::TypeId;
use heapless::Vec;
use stak_vm::{Error, Memory, Number, PrimitiveSet, Type, Value};

const MAXIMUM_ARGUMENT_COUNT: usize = 16;

type ArgumentVec<T> = Vec<T, MAXIMUM_ARGUMENT_COUNT>;

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

    fn convert_from_scheme(value: Value, type_id: TypeId) -> Option<any_fn::Value> {
        if type_id == TypeId::of::<f32>() {
            Some(any_fn::value(value.assume_number().to_f64() as f32))
        } else if type_id == TypeId::of::<f64>() {
            Some(any_fn::value(value.assume_number().to_f64()))
        } else if type_id == TypeId::of::<i8>() {
            Some(any_fn::value(value.assume_number().to_i64() as i8))
        } else if type_id == TypeId::of::<u8>() {
            Some(any_fn::value(value.assume_number().to_i64() as u8))
        } else if type_id == TypeId::of::<isize>() {
            Some(any_fn::value(value.assume_number().to_i64() as isize))
        } else if type_id == TypeId::of::<usize>() {
            Some(any_fn::value(value.assume_number().to_i64() as usize))
        } else {
            None
        }
    }

    fn convert_into_scheme(
        &mut self,
        memory: &mut Memory,
        value: any_fn::Value,
    ) -> Result<Value, DynamicError> {
        Ok(if value.type_id()? == TypeId::of::<bool>() {
            memory.boolean(value.downcast::<bool>()?).into()
        } else if value.type_id()? == TypeId::of::<f64>() {
            Number::from_f64(value.downcast::<f64>()?).into()
        } else {
            let index = self
                .values
                .iter()
                .position(Option::is_none)
                .ok_or(Error::OutOfMemory)?;

            self.values[index] = Some(value);

            let cons = memory.allocate(
                Number::from_i64(index as _).into(),
                memory.null().set_tag(Type::Foreign as _).into(),
            )?;

            cons.into()
        })
    }
}

impl<const N: usize> PrimitiveSet for DynamicPrimitiveSet<'_, '_, N> {
    type Error = DynamicError;

    fn operate(&mut self, memory: &mut Memory, primitive: usize) -> Result<(), Self::Error> {
        let function = self
            .functions
            .get_mut(primitive)
            .ok_or(Error::IllegalPrimitive)?;

        let mut arguments = (0..function.arity())
            .map(|_| memory.pop())
            .collect::<ArgumentVec<_>>();
        arguments.reverse();

        let cloned_arguments = arguments
            .iter()
            .enumerate()
            .map(|(index, &value)| {
                Self::convert_from_scheme(value, function.parameter_types()[index])
            })
            .collect::<ArgumentVec<_>>();

        let mut copied_arguments = ArgumentVec::new();

        for &value in &arguments {
            let value = if value.is_cons() && memory.cdr_value(value).tag() == Type::Foreign as _ {
                Some(
                    self.values
                        .get(memory.car(value.assume_cons()).assume_number().to_i64() as usize)
                        .ok_or(DynamicError::ObjectIndex)?
                        .as_ref()
                        .ok_or(DynamicError::ObjectIndex)?,
                )
            } else {
                None
            };

            copied_arguments
                .push(value)
                .map_err(|_| Error::ArgumentCount)?;
        }

        let value = function.call(
            copied_arguments
                .into_iter()
                .enumerate()
                .map(|(index, value)| {
                    if let Some(value) = &cloned_arguments[index] {
                        Ok(value)
                    } else {
                        value.ok_or(DynamicError::ForeignValueExpected)
                    }
                })
                .collect::<Result<ArgumentVec<_>, DynamicError>>()?
                .as_slice(),
        )?;

        let value = self.convert_into_scheme(memory, value)?;
        memory.push(value)?;

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use any_fn::{r#fn, Ref};

    struct Foo {
        bar: usize,
    }

    impl Foo {
        const fn new(bar: usize) -> Self {
            Self { bar }
        }

        const fn bar(&self) -> usize {
            self.bar
        }

        fn baz(&mut self, value: usize) {
            self.bar += value;
        }
    }

    #[test]
    fn create() {
        let mut functions = [
            r#fn(Foo::new),
            r#fn::<(Ref<_>,), _>(Foo::bar),
            r#fn(Foo::baz),
        ];

        DynamicPrimitiveSet::<0>::new(&mut functions);
    }
}
