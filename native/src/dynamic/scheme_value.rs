use super::DynamicError;
use stak_vm::Memory;
use stak_vm::{Number, Value};

/// A trait to convert Rust values from and into Scheme values.
pub trait SchemeValue: Sized {
    /// Converts a Scheme value into a Rust value.
    fn from_scheme(memory: &Memory, value: Value) -> Option<Self>;

    /// Converts a Rust value into a Scheme value.
    fn into_scheme(memory: &mut Memory, value: Self) -> Result<Value, DynamicError>;
}

impl SchemeValue for bool {
    fn from_scheme(memory: &Memory, value: Value) -> Option<Self> {
        Some(value == memory.boolean(false).into())
    }

    fn into_scheme(memory: &mut Memory, value: Self) -> Result<Value, DynamicError> {
        Ok(memory.boolean(value).into())
    }
}

macro_rules! implement_integer {
    ($type:ty) => {
        impl SchemeValue for $type {
            fn from_scheme(_memory: &Memory, value: Value) -> Option<Self> {
                Some(value.assume_number().to_i64() as _)
            }

            fn into_scheme(_memory: &mut Memory, value: Self) -> Result<Value, DynamicError> {
                Ok(Number::from_i64(value as _).into())
            }
        }
    };
}

implement_integer!(u8);
implement_integer!(i8);
implement_integer!(u16);
implement_integer!(i16);
implement_integer!(u32);
implement_integer!(i32);
implement_integer!(u64);
implement_integer!(i64);
implement_integer!(usize);
implement_integer!(isize);

macro_rules! implement_float {
    ($type:ty) => {
        impl SchemeValue for $type {
            fn from_scheme(_memory: &Memory, value: Value) -> Option<Self> {
                Some(value.assume_number().to_f64() as _)
            }

            fn into_scheme(_memory: &mut Memory, value: Self) -> Result<Value, DynamicError> {
                Ok(Number::from_f64(value as _).into())
            }
        }
    };
}

implement_float!(f32);
implement_float!(f64);
