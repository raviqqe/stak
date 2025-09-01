use super::DynamicError;
use alloc::string::String;
use stak_vm::{Heap, Memory, Number, Type, Value};

/// A trait to convert Rust values from and into Scheme values.
pub trait SchemeValue<H>: Sized {
    /// Converts a Scheme value into a Rust value.
    fn from_scheme(memory: &Memory<H>, value: Value) -> Result<Option<Self>, DynamicError>;

    /// Converts a Rust value into a Scheme value.
    fn into_scheme(self, memory: &mut Memory<H>) -> Result<Value, DynamicError>;
}

impl<H: Heap> SchemeValue<H> for bool {
    fn from_scheme(memory: &Memory<H>, value: Value) -> Result<Option<Self>, DynamicError> {
        Ok(Some(value == memory.boolean(false)?.into()))
    }

    fn into_scheme(self, memory: &mut Memory<H>) -> Result<Value, DynamicError> {
        Ok(memory.boolean(self)?.into())
    }
}

macro_rules! implement_integer {
    ($type:ty) => {
        impl<H> SchemeValue<H> for $type {
            fn from_scheme(
                _memory: &Memory<H>,
                value: Value,
            ) -> Result<Option<Self>, DynamicError> {
                Ok(Some(value.assume_number().to_i64() as _))
            }

            fn into_scheme(self, _memory: &mut Memory<H>) -> Result<Value, DynamicError> {
                Ok(Number::from_i64(self as _).into())
            }
        }
    };
}

implement_integer!(i8);
implement_integer!(u8);
implement_integer!(u16);
implement_integer!(i16);
implement_integer!(i32);
implement_integer!(u32);
implement_integer!(i64);
implement_integer!(u64);
implement_integer!(isize);
implement_integer!(usize);

macro_rules! implement_float {
    ($type:ty) => {
        impl<H> SchemeValue<H> for $type {
            fn from_scheme(
                _memory: &Memory<H>,
                value: Value,
            ) -> Result<Option<Self>, DynamicError> {
                Ok(Some(value.assume_number().to_f64() as _))
            }

            fn into_scheme(self, _memory: &mut Memory<H>) -> Result<Value, DynamicError> {
                Ok(Number::from_f64(self as _).into())
            }
        }
    };
}

implement_float!(f32);
implement_float!(f64);

impl<H: Heap> SchemeValue<H> for String {
    fn from_scheme(memory: &Memory<H>, value: Value) -> Result<Option<Self>, DynamicError> {
        let cons = value.assume_cons();
        let mut string = Self::with_capacity(memory.car(cons)?.assume_number().to_i64() as _);
        let mut cons = memory.cdr(cons)?.assume_cons();

        while cons != memory.null()? {
            let Some(character) = char::from_u32(memory.car(cons)?.assume_number().to_i64() as _)
            else {
                return Ok(None);
            };
            string.push(character);
            cons = memory.cdr(cons)?.assume_cons();
        }

        Ok(Some(string))
    }

    fn into_scheme(self, memory: &mut Memory<H>) -> Result<Value, DynamicError> {
        let mut length = 0;
        let mut cons = memory.null()?;

        for character in self.chars().rev() {
            cons = memory.cons(Number::from_i64(character as _).into(), cons)?;
            length += 1;
        }

        Ok(memory
            .allocate(
                Number::from_i64(length).into(),
                cons.set_tag(Type::String as _).into(),
            )?
            .into())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    mod string {
        use super::*;

        #[test]
        fn ascii() {
            let mut memory = Memory::new([Default::default(); 256]).unwrap();
            let string = "tomato";

            let value = String::from(string).into_scheme(&mut memory).unwrap();

            assert_eq!(
                &String::from_scheme(&memory, value).unwrap().unwrap(),
                string
            );
        }

        #[test]
        fn unicode() {
            let mut memory = Memory::new([Default::default(); 256]).unwrap();
            let string = "あ阿😄";

            let value = String::from(string).into_scheme(&mut memory).unwrap();

            assert_eq!(
                &String::from_scheme(&memory, value).unwrap().unwrap(),
                string
            );
        }
    }
}
