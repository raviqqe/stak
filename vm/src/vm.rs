use crate::{value::Value, Error};
use alloc::{vec, vec::Vec};

const CONS_FIELD_COUNT: usize = 2;
const ZERO: Value = Value::Number(0);

#[allow(dead_code)]
#[derive(Default)]
pub struct Vm<const N: usize> {
    heap: Vec<Value>,
}

impl<const N: usize> Vm<N> {
    const SPACE_SIZE: usize = N * CONS_FIELD_COUNT;
    const HEAP_SIZE: usize = Self::SPACE_SIZE * 2;

    pub fn new() -> Self {
        Self {
            heap: vec![ZERO; Self::HEAP_SIZE],
        }
    }

    pub fn run(&self) -> Result<(), Error> {
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const HEAP_SIZE: usize = 1 << 8;

    #[test]
    fn run_nothing() {
        Vm::<HEAP_SIZE>::new().run().unwrap();
    }
}
