use stak_vm::{Error, Memory, PrimitiveSet, Type, Value};

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

    fn operate_top<'a>(
        memory: &mut Memory<'a>,
        operate: impl Fn(&Memory<'a>, Value) -> Value,
    ) -> Result<(), Error> {
        let x = memory.pop();
        memory.push(operate(memory, x))?;
        Ok(())
    }
}

impl PrimitiveSet for TypeCheckPrimitiveSet {
    type Error = Error;

    fn operate(&mut self, memory: &mut Memory, primitive: usize) -> Result<(), Self::Error> {
        match primitive {
            TypeCheckPrimitive::NULL => Self::operate_top(memory, |memory, value| {
                memory.boolean(value == memory.null().into()).into()
            })?,
            TypeCheckPrimitive::PAIR => Self::operate_top(memory, |memory, value| {
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
