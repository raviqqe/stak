use crate::{value::Value, Error};

const CONS_FIELD_COUNT: usize = 2;
const MAX_VALUE_COUNT: usize = 1 << 14;
const SPACE_SIZE: usize = MAX_VALUE_COUNT * CONS_FIELD_COUNT;
const HEAP_SIZE: usize = SPACE_SIZE * 2;
#[allow(dead_code)]
const HEAP_TOP: usize = HEAP_SIZE;

const ZERO: Value = Value::Number(0);

#[allow(dead_code)]
pub struct Vm {
    heap: Vec<Value>,
    heap_size: usize,
}

impl Vm {
    pub fn new(heap_size: usize) -> Self {
        Self {
            heap: vec![ZERO; HEAP_SIZE],
            heap_size,
        }
    }

    pub fn run(&self) -> Result<(), Error> {
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn run_nothing() {
        Vm::new(0).run().unwrap();
    }
}
