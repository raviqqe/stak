use crate::{cons::Cons, number::Number, value::Value, Error};
use alloc::{vec, vec::Vec};
use core::fmt::{self, Display, Formatter};

const CONS_FIELD_COUNT: usize = 2;
const ZERO: Number = Number::new(0);

#[allow(dead_code)]
#[derive(Debug)]
pub struct Vm<const N: usize> {
    stack: Value,
    heap: Vec<Value>,
    allocation_index: usize,
    odd_gc: bool,
}

#[allow(dead_code)]
impl<const N: usize> Vm<N> {
    const SPACE_SIZE: usize = N * CONS_FIELD_COUNT;
    const HEAP_SIZE: usize = Self::SPACE_SIZE * 2;
    const HEAP_MIDDLE: usize = Self::SPACE_SIZE;
    const HEAP_TOP: usize = Self::HEAP_SIZE;
    const GC_COPIED_CAR: Cons = Cons::new(Self::HEAP_SIZE as u64);

    pub fn new() -> Self {
        Self {
            stack: ZERO.into(),
            heap: vec![ZERO.into(); Self::HEAP_SIZE],
            allocation_index: 0,
            odd_gc: false,
        }
    }

    pub fn run(&mut self) -> Result<(), Error> {
        Ok(())
    }

    fn append(&mut self, car: Value, cdr: Value) -> Cons {
        let cons = self.allocate();

        *self.car_mut(cons) = car;
        *self.cdr_mut(cons) = cdr;

        cons
    }

    fn push(&mut self, value: Value) {
        self.stack = self.append(value, self.stack).into();
    }

    fn pop(&mut self) -> Option<Value> {
        if let Some(cons) = self.stack.to_cons() {
            let value = self.car(cons);
            self.stack = self.cdr(cons);
            Some(value)
        } else {
            None
        }
    }

    pub fn allocate(&mut self) -> Cons {
        let cons = self.allocate_raw();

        debug_assert!(self.allocation_index <= Self::SPACE_SIZE);

        if self.allocation_index == Self::SPACE_SIZE {
            self.collect_garbages();
        }

        cons
    }

    fn allocate_raw(&mut self) -> Cons {
        let cons = Cons::new((self.allocation_start() + self.allocation_index) as u64);
        self.allocation_index += CONS_FIELD_COUNT;
        cons
    }

    fn allocation_start(&self) -> usize {
        if self.odd_gc {
            Self::HEAP_MIDDLE
        } else {
            0
        }
    }

    fn allocation_end(&self) -> usize {
        if self.odd_gc {
            Self::HEAP_TOP
        } else {
            Self::HEAP_MIDDLE
        }
    }

    fn collect_garbages(&mut self) {
        self.allocation_index = 0;
        self.odd_gc = !self.odd_gc;

        self.stack = self.copy_value(self.stack);

        for index in self.allocation_start()..self.allocation_end() {
            self.heap[index] = self.copy_value(self.heap[index]);
        }
    }

    fn copy_value(&mut self, value: Value) -> Value {
        if let Some(cons) = value.to_cons() {
            if self.car(cons) == Self::GC_COPIED_CAR.into() {
                // Get a forward pointer.
                self.cdr(cons)
            } else {
                let copy = self.allocate_raw();

                *self.car_mut(copy) = self.car(cons);
                *self.cdr_mut(copy) = self.cdr(cons);

                *self.car_mut(cons) = Self::GC_COPIED_CAR.into();
                // Set a forward pointer.
                *self.cdr_mut(cons) = copy.into();

                copy.into()
            }
        } else {
            value
        }
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

        let list = vm.append(Number::new(1).into(), ZERO.into());

        insta::assert_display_snapshot!(vm);

        let list = vm.append(Number::new(2).into(), list.into());

        insta::assert_display_snapshot!(vm);

        vm.append(Number::new(3).into(), list.into());

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

            vm.push(Number::new(42).into());

            assert_eq!(vm.pop(), Some(Number::new(42).into()));
        }

        #[test]
        fn push_and_pop_twice() {
            let mut vm = Vm::<HEAP_SIZE>::new();

            vm.push(Number::new(1).into());
            vm.push(Number::new(2).into());

            assert_eq!(vm.pop(), Some(Number::new(2).into()));
            assert_eq!(vm.pop(), Some(Number::new(1).into()));
        }
    }

    mod garbage_collection {
        use super::*;

        #[test]
        fn collect_cons() {
            let mut vm = Vm::<HEAP_SIZE>::new();

            vm.allocate();
            vm.collect_garbages();

            insta::assert_display_snapshot!(vm);
        }

        #[test]
        fn collect_stack() {
            let mut vm = Vm::<HEAP_SIZE>::new();

            vm.push(Number::new(42).into());
            vm.collect_garbages();

            insta::assert_display_snapshot!(vm);
        }

        #[test]
        fn collect_cycle() {
            let mut vm = Vm::<HEAP_SIZE>::new();

            let cons = vm.allocate();
            *vm.cdr_mut(cons) = cons.into();

            vm.collect_garbages();

            insta::assert_display_snapshot!(vm);
        }
    }
}
