use crate::{cons::Cons, number::Number, value::Value, Error};
use alloc::{vec, vec::Vec};
use core::fmt::{self, Display, Formatter};

const CONS_FIELD_COUNT: usize = 2;
const ZERO: Number = Number::new(0);
const GC_COPIED_CAR: Cons = Cons::new(i64::MAX as u64);

#[derive(Debug)]
pub struct Vm<const N: usize> {
    heap: Vec<Value>,
    stack: Value,
    allocation_index: usize,
    gc_inverse: bool,
}

impl<const N: usize> Vm<N> {
    const SPACE_SIZE: usize = N * CONS_FIELD_COUNT;
    const HEAP_SIZE: usize = Self::SPACE_SIZE * 2;
    const HEAP_MIDDLE: usize = Self::SPACE_SIZE;

    pub fn new() -> Self {
        Self {
            heap: vec![ZERO.into(); Self::HEAP_SIZE],
            stack: ZERO.into(),
            allocation_index: 0,
            gc_inverse: false,
        }
    }

    pub fn run(&mut self) -> Result<(), Error> {
        Ok(())
    }

    fn append(&mut self, car: Value, cdr: Value) -> Result<Cons, Error> {
        let cons = self.allocate()?;

        *self.car_mut(cons) = car;
        *self.cdr_mut(cons) = cdr;

        Ok(cons)
    }

    pub fn push(&mut self, value: Value) -> Result<(), Error> {
        self.stack = self.append(value, self.stack)?.into();

        Ok(())
    }

    pub fn pop(&mut self) -> Option<Value> {
        if let Some(cons) = self.stack.to_cons() {
            let value = self.car(cons);
            self.stack = self.cdr(cons);
            Some(value)
        } else {
            None
        }
    }

    pub fn allocate(&mut self) -> Result<Cons, Error> {
        let cons = self.allocate_raw();

        debug_assert!(self.allocation_index <= Self::SPACE_SIZE);

        if self.allocation_index == Self::SPACE_SIZE {
            self.collect_garbages();

            if self.allocation_index == Self::SPACE_SIZE {
                return Err(Error::OutOfMemory);
            }
        }

        Ok(cons)
    }

    fn allocate_raw(&mut self) -> Cons {
        let cons = Cons::new((self.allocation_start() + self.allocation_index) as u64);
        self.allocation_index += CONS_FIELD_COUNT;
        cons
    }

    fn allocation_start(&self) -> usize {
        if self.gc_inverse {
            Self::HEAP_MIDDLE
        } else {
            0
        }
    }

    fn allocation_end(&self) -> usize {
        self.allocation_start() + Self::SPACE_SIZE
    }

    fn car(&self, cons: Cons) -> Value {
        self.heap[cons.index()]
    }

    fn cdr(&self, cons: Cons) -> Value {
        self.heap[cons.index() + 1]
    }

    fn car_mut(&mut self, cons: Cons) -> &mut Value {
        &mut self.heap[cons.index()]
    }

    fn cdr_mut(&mut self, cons: Cons) -> &mut Value {
        &mut self.heap[cons.index() + 1]
    }

    // Garbage collection

    fn collect_garbages(&mut self) {
        self.allocation_index = 0;
        self.gc_inverse = !self.gc_inverse;

        self.stack = self.copy_value(self.stack);

        for index in self.allocation_start()..self.allocation_end() {
            self.heap[index] = self.copy_value(self.heap[index]);
        }
    }

    fn copy_value(&mut self, value: Value) -> Value {
        if let Some(cons) = value.to_cons() {
            if self.car(cons) == GC_COPIED_CAR.into() {
                // Get a forward pointer.
                self.cdr(cons)
            } else {
                let copy = self.allocate_raw();

                *self.car_mut(copy) = self.car(cons);
                *self.cdr_mut(copy) = self.cdr(cons);

                *self.car_mut(cons) = GC_COPIED_CAR.into();
                // Set a forward pointer.
                *self.cdr_mut(cons) = copy.into();

                copy.into()
            }
        } else {
            value
        }
    }
}

impl<const N: usize> Default for Vm<N> {
    fn default() -> Self {
        Self::new()
    }
}

impl<const N: usize> Display for Vm<N> {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        for index in 0..self.allocation_index / 2 {
            let cons = Cons::new((self.allocation_start() + 2 * index) as u64);

            writeln!(formatter, "{} {}", self.car(cons), self.cdr(cons))?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::format;

    const HEAP_SIZE: usize = 1 << 4;

    #[test]
    fn create() {
        let vm = Vm::<HEAP_SIZE>::new();

        insta::assert_display_snapshot!(vm);
    }

    #[test]
    fn run_nothing() {
        let mut vm = Vm::<HEAP_SIZE>::new();

        vm.run().unwrap();

        insta::assert_display_snapshot!(vm);
    }

    #[test]
    fn create_list() {
        let mut vm = Vm::<HEAP_SIZE>::new();

        let list = vm.append(Number::new(1).into(), ZERO.into()).unwrap();

        insta::assert_display_snapshot!(vm);

        let list = vm.append(Number::new(2).into(), list.into()).unwrap();

        insta::assert_display_snapshot!(vm);

        vm.append(Number::new(3).into(), list.into()).unwrap();

        insta::assert_display_snapshot!(vm);
    }

    mod stack {
        use super::*;

        #[test]
        fn pop_nothing() {
            let mut vm = Vm::<HEAP_SIZE>::new();

            assert_eq!(vm.pop(), None);
        }

        #[test]
        fn push_and_pop() {
            let mut vm = Vm::<HEAP_SIZE>::new();

            vm.push(Number::new(42).into()).unwrap();

            assert_eq!(vm.pop(), Some(Number::new(42).into()));
        }

        #[test]
        fn push_and_pop_twice() {
            let mut vm = Vm::<HEAP_SIZE>::new();

            vm.push(Number::new(1).into()).unwrap();
            vm.push(Number::new(2).into()).unwrap();

            assert_eq!(vm.pop(), Some(Number::new(2).into()));
            assert_eq!(vm.pop(), Some(Number::new(1).into()));
        }
    }

    mod garbage_collection {
        use super::*;

        #[test]
        fn collect_cons() {
            let mut vm = Vm::<HEAP_SIZE>::new();

            vm.allocate().unwrap();
            vm.collect_garbages();

            insta::assert_display_snapshot!(vm);
        }

        #[test]
        fn collect_stack() {
            let mut vm = Vm::<HEAP_SIZE>::new();

            vm.push(Number::new(42).into()).unwrap();
            vm.collect_garbages();

            insta::assert_display_snapshot!(vm);
        }

        #[test]
        fn collect_deep_stack() {
            let mut vm = Vm::<HEAP_SIZE>::new();

            vm.push(Number::new(1).into()).unwrap();
            vm.push(Number::new(2).into()).unwrap();
            vm.collect_garbages();

            insta::assert_display_snapshot!(vm);
        }

        #[test]
        fn collect_cycle() {
            let mut vm = Vm::<HEAP_SIZE>::new();

            let cons = vm.allocate().unwrap();
            *vm.cdr_mut(cons) = cons.into();

            vm.collect_garbages();

            insta::assert_display_snapshot!(vm);
        }
    }
}
