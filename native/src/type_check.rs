use core::convert::Infallible;
use stak_vm::Memory;

pub enum TypeCheckPrimitive {
    Null,
    Pair,
}

impl TypeCheckPrimitive {
    pub const NULL: usize = Self::Null as _;
    pub const PAIR: usize = Self::Pair as _;
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
    type Error = Infallible;

    fn operate(&mut self, memory: &mut Memory, primitive: usize) -> Result<(), Self::Error> {
        match primitive {
            TypeCheckPrimitive::NULL => {
                let value = memory.pop();
                memory.push(memory.boolean(value == memory.null().into()).into())?;
            }
            TypeCheckPrimitive::PAIR => Self::check_type(memory, Type::Pair)?,
        }

        Ok(())
    }
}
