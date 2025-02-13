use stak_vm::{Error, Memory, PrimitiveSet, Type};

/// An equality primitive.
pub enum EqualPrimitive {
    /// An `eqv` procedure.
    Eqv,
}

impl EqualPrimitive {
    const EQV: usize = Self::Eqv as _;
}

/// An equality primitive set.
#[derive(Debug, Default)]
pub struct EqualPrimitiveSet {}

impl EqualPrimitiveSet {
    /// Creates a primitive set.
    pub fn new() -> Self {
        Self::default()
    }
}

impl PrimitiveSet for EqualPrimitiveSet {
    type Error = Error;

    fn operate(&mut self, memory: &mut Memory, primitive: usize) -> Result<(), Self::Error> {
        match primitive {
            EqualPrimitive::EQV => {
                let [x, y] = memory.pop_many();

                self.push(memory.boolean(
                    x == y
                        || if let (Some(x), Some(y)) = (x.to_cons(), y.to_cons()) {
                            memory.car(x) == memory.car(y)
                        } else {
                            false
                        },
                ))?;
            }
            _ => return Err(Error::IllegalPrimitive),
        }

        Ok(())
    }
}
