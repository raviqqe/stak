use crate::primitive::Primitive;
use libm::{acos, asin, atan, cos, exp, log, sin, sqrt, tan};
use stak_vm::{Error, Memory, Number, PrimitiveSet};
use winter_maybe_async::maybe_async;

/// A primitive set for inexact number operations.
#[derive(Debug, Default)]
pub struct InexactPrimitiveSet {}

impl InexactPrimitiveSet {
    /// Creates a primitive set.
    pub fn new() -> Self {
        Self::default()
    }
}

impl InexactPrimitiveSet {
    fn operate_unary(memory: &mut Memory<'_>, calculate: fn(f64) -> f64) -> Result<(), Error> {
        memory.operate_unary(|x| Number::from_f64(calculate(x.to_f64())))
    }

    fn operate_condition(memory: &mut Memory<'_>, calculate: fn(f64) -> bool) -> Result<(), Error> {
        memory.operate_top(|memory, x| {
            Ok(memory
                .boolean(calculate(x.assume_number().to_f64()))?
                .into())
        })
    }
}

impl PrimitiveSet for InexactPrimitiveSet {
    type Error = Error;

    #[maybe_async]
    fn operate(&mut self, memory: &mut Memory<'_>, primitive: usize) -> Result<(), Self::Error> {
        match primitive {
            Primitive::EXPONENTIATION => Self::operate_unary(memory, exp)?,
            Primitive::LOGARITHM => Self::operate_unary(memory, log)?,
            Primitive::INFINITE => Self::operate_condition(memory, f64::is_infinite)?,
            Primitive::NAN => Self::operate_condition(memory, f64::is_nan)?,
            Primitive::SQRT => Self::operate_unary(memory, sqrt)?,
            Primitive::COS => Self::operate_unary(memory, cos)?,
            Primitive::SIN => Self::operate_unary(memory, sin)?,
            Primitive::TAN => Self::operate_unary(memory, tan)?,
            Primitive::ACOS => Self::operate_unary(memory, acos)?,
            Primitive::ASIN => Self::operate_unary(memory, asin)?,
            Primitive::ATAN => Self::operate_unary(memory, atan)?,
            _ => return Err(Error::IllegalPrimitive),
        }

        Ok(())
    }
}
