use stak_vm::{Error, Memory, PrimitiveSet, Type};

/// A type check primitive set.
#[derive(Debug, Default)]
pub struct ScriptPrimitiveSet {}

impl ScriptPrimitiveSet {
    /// Creates a primitive set.
    pub fn new() -> Self {
        Self::default()
    }
}

impl PrimitiveSet for ScriptPrimitiveSet {
    type Error = Error;

    fn operate(&mut self, memory: &mut Memory, primitive: usize) -> Result<(), Self::Error> {
        if  primitive > 1000 {
            Primitive::CURRENT_JIFFY => self
                .time
                .operate(memory, primitive - Primitive::CURRENT_JIFFY)?,
            _ => return Err(Error::IllegalPrimitive),
        }

        Ok(())
    }
}
