use crate::cons::Cons;
use crate::number::Number;
use crate::{value::Value, Error};
use alloc::{vec, vec::Vec};

const CONS_FIELD_COUNT: usize = 2;
const ZERO: Number = Number::new(0);

#[allow(dead_code)]
#[derive(Debug, Default)]
pub struct Vm<const N: usize> {
    heap: Vec<Value>,
    allocation_index: usize,
    allocation_limit: usize,
}

#[allow(dead_code)]
impl<const N: usize> Vm<N> {
    const SPACE_SIZE: usize = N * CONS_FIELD_COUNT;
    const HEAP_SIZE: usize = Self::SPACE_SIZE * 2;
    const HEAP_MIDDLE: usize = Self::SPACE_SIZE;
    const HEAP_TOP: usize = Self::HEAP_SIZE;

    pub fn new() -> Self {
        Self {
            heap: vec![ZERO.into(); Self::HEAP_SIZE],
            allocation_index: 0,
            allocation_limit: Self::HEAP_MIDDLE,
        }
    }

    pub fn car(&self, index: Number) -> Value {
        self.heap[index.to_usize()]
    }

    pub fn cdr(&self, index: Number) -> Value {
        self.heap[index.to_usize() + 1]
    }

    fn car_mut(&mut self, cons: Cons) -> &mut Value {
        &mut self.heap[cons.index() as usize]
    }

    fn cdr_mut(&mut self, cons: Cons) -> &mut Value {
        &mut self.heap[cons.index() as usize + 1]
    }

    pub fn allocate(&mut self) -> Cons {
        if self.allocation_index == self.allocation_limit {
            todo!("gc")
        }

        let cons = Cons::new(self.allocation_index as u64);
        self.allocation_index += CONS_FIELD_COUNT;
        cons
    }

    pub fn append(&mut self, value: Value, cons: Value) {
        let new = self.allocate();

        *self.car_mut(new) = value.into();
        *self.cdr_mut(new) = cons.into();
    }

    pub fn run(&self) -> Result<(), Error> {
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::format;

    const HEAP_SIZE: usize = 1 << 8;

    #[test]
    fn run_nothing() {
        Vm::<HEAP_SIZE>::new().run().unwrap();
    }

    #[test]
    fn create_list() {
        let mut vm = Vm::<HEAP_SIZE>::new();

        vm.append(Number::new(1).into(), ZERO.into());

        insta::assert_debug_snapshot!(vm);
    }
}
