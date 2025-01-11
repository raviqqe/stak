use stak_vm::{Error, Memory, PrimitiveSet, Type};

/// A type check primitive.
pub enum TypeCheckPrimitive {
    /// A null type check.
    Null,
    /// A pair type check.
    Pair,
}

impl TypeCheckPrimitive {
    const NULL: usize = Self::Null as _;
    const PAIR: usize = Self::Pair as _;
}

/// A type check primitive set.
#[derive(Debug, Default)]
pub struct TypeCheckPrimitiveSet {}

impl TypeCheckPrimitiveSet {
    /// Creates a primitive set.
    pub fn new() -> Self {
        Self::default()
    }
}

impl PrimitiveSet for TypeCheckPrimitiveSet {
    type Error = Error;

    fn operate(&mut self, memory: &mut Memory, primitive: usize) -> Result<(), Self::Error> {
        match primitive {
            TypeCheckPrimitive::NULL => memory.operate_top(|memory, value| {
                memory.boolean(value == memory.null().into()).into()
            })?,
            TypeCheckPrimitive::PAIR => memory.operate_top(|memory, value| {
                memory
                    .boolean(
                        value
                            .to_cons()
                            .map(|cons| memory.cdr(cons).tag() == Type::Pair as _)
                            .unwrap_or_default(),
                    )
                    .into()
            })?,
            _ => return Err(Error::IllegalPrimitive),
        }

        Ok(())
    }
}
