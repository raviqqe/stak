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
    fn operate_unary(
        &mut self,
        memory: &mut Memory<'_>,
        calculate: fn(f64) -> f64,
    ) -> Result<(), Error> {
        memory.operate_unary(|x| Number::from_f64(calculate(x.to_f64())))
    }

    fn operate_condition(
        &mut self,
        memory: &mut Memory<'_>,
        calculate: fn(f64) -> bool,
    ) -> Result<(), Error> {
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
            Primitive::EXPONENTIATION => self.operate_unary(memory, exp)?,
            Primitive::LOGARITHM => self.operate_unary(memory, log)?,
            Primitive::INFINITE => self.operate_condition(memory, f64::is_infinite)?,
            Primitive::NAN => self.operate_condition(memory, f64::is_nan)?,
            Primitive::SQRT => self.operate_unary(memory, sqrt)?,
            Primitive::COS => self.operate_unary(memory, cos)?,
            Primitive::SIN => self.operate_unary(memory, sin)?,
            Primitive::TAN => self.operate_unary(memory, tan)?,
            Primitive::ACOS => self.operate_unary(memory, acos)?,
            Primitive::ASIN => self.operate_unary(memory, asin)?,
            Primitive::ATAN => self.operate_unary(memory, atan)?,
            _ => return Err(Error::IllegalPrimitive),
        }

        Ok(())
    }
}
