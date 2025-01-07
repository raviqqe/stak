use crate::primitive::Primitive;
use libm::{exp, log};
use stak_vm::{Error, Memory, Number, PrimitiveSet};

/// A primitive set for inexact number operations.
#[derive(Debug, Default)]
pub struct SmallPrimitiveSet {}

impl SmallPrimitiveSet {
    /// Creates a primitive set.
    pub fn new() -> Self {
        Self::default()
    }
}

impl PrimitiveSet for SmallPrimitiveSet {
    type Error = Error;

    fn operate(&mut self, memory: &mut Memory, primitive: usize) -> Result<(), Self::Error> {
        match primitive {
            Primitive::EXPONENTIATION => {
                memory.operate_unary(|x| Number::from_f64(exp(x.to_f64())))?
            }
            Primitive::LOGARITHM => memory.operate_unary(|x| Number::from_f64(log(x.to_f64())))?,
            _ => return Err(stak_vm::Error::IllegalPrimitive.into()),
        }

        Ok(())
    }
}
