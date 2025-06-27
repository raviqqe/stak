use stak_vm::{Error, Memory, PrimitiveSet, Type};
use winter_maybe_async::maybe_async;

/// An equality primitive.
pub enum EqualPrimitive {
    /// An `eqv` procedure.
    Eqv,
    /// A primitive to check equality of rib inners.
    EqualInner,
}

impl EqualPrimitive {
    const EQV: usize = Self::Eqv as _;
    const EQUAL_INNER: usize = Self::EqualInner as _;
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

    #[maybe_async]
    fn operate(&mut self, memory: &mut Memory<'_>, primitive: usize) -> Result<(), Self::Error> {
        match primitive {
            EqualPrimitive::EQV => {
                let [x, y] = memory.pop_many()?;

                memory.push(
                    memory
                        .boolean(
                            x == y
                                || if let (Some(x), Some(y)) = (x.to_cons(), y.to_cons()) {
                                    memory.cdr(x)?.tag() == Type::Character as _
                                        && memory.cdr(y)?.tag() == Type::Character as _
                                        && memory.car(x) == memory.car(y)
                                } else {
                                    false
                                },
                        )?
                        .into(),
                )?;
            }
            EqualPrimitive::EQUAL_INNER => {
                let [x, y] = memory.pop_many()?;

                memory.push(
                    memory
                        .boolean(if let (Some(x), Some(y)) = (x.to_cons(), y.to_cons()) {
                            // - Optimize checks for unique values.
                            // - Optimize checks for strings and vectors where `car`s are integers.
                            memory.cdr(x)?.tag() == memory.cdr(y)?.tag()
                                && ![Type::Boolean as _, Type::Null as _, Type::Symbol as _]
                                    .contains(&memory.cdr(x)?.tag())
                                && (memory.car(x)?.is_cons() || memory.car(x)? == memory.car(y)?)
                        } else {
                            false
                        })?
                        .into(),
                )?;
            }

            _ => return Err(Error::IllegalPrimitive),
        }

        Ok(())
    }
}
