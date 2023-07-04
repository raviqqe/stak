use crate::{cons::Cons, number::Number, primitive::Primitive, value::Value, Error};
use core::{
    fmt::{self, Display, Formatter},
    ops::{Add, Div, Mul, Sub},
};

const CONS_FIELD_COUNT: usize = 2;
const ZERO: Number = Number::new(0);
const GC_COPIED_CAR: Cons = Cons::new(i64::MAX as u64);

#[derive(Debug)]
pub struct Vm<const N: usize> {
    heap: [Value; N],
    stack: Value,
    allocation_index: usize,
    gc_inverse: bool,
}

impl<const N: usize> Vm<N> {
    const SPACE_SIZE: usize = N / 2;

    pub fn new() -> Self {
        Self {
            heap: [ZERO.into(); N],
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

    pub fn pop(&mut self) -> Result<Value, Error> {
        if let Some(cons) = self.stack.to_cons() {
            let value = self.car(cons);
            self.stack = self.cdr(cons);
            Ok(value)
        } else {
            Err(Error::StackUnderflow)
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
            N / 2
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

    fn boolean(&self, value: bool) -> Value {
        todo!()
    }

    // Primitive operations

    fn operate_primitive(&mut self, primitive: u8) -> Result<(), Error> {
        match primitive {
            Primitive::CONS => {
                let cons = self.allocate()?;
                *self.car_mut(cons) = self.pop()?;
                *self.cdr_mut(cons) = self.pop()?;
                self.push(cons.into())?;
            }
            Primitive::ID => {
                let x = self.pop()?;
                self.push(x)?;
            }
            Primitive::POP => {
                self.pop()?;
            }
            Primitive::SKIP => {
                let x = self.pop()?;
                self.pop()?;
                self.push(x)?;
            }
            Primitive::CLOSE => {
                let cons = self.allocate()?;

                *self.car_mut(cons) = self.pop()?;
                *self.cdr_mut(cons) = self.stack;

                self.push(cons.into())?;
            }
            Primitive::IS_CONS => {
                let x = self.pop()?;
                self.push(self.boolean(x.is_cons()));
            }
            Primitive::CAR => {
                let x = self.pop()?;
                self.push(self.car(x))?;
            }
            Primitive::CDR => {
                let x = self.pop();
                self.push(self.cdr(x))?;
            }
            Primitive::SET_CAR => {
                let x = self.pop()?;
                let y = self.pop()?;
                *self.car_mut(x) = y;
                self.push(y)?;
            }
            Primitive::SET_CDR => {
                let x = self.pop()?;
                let y = self.pop()?;
                *self.cdr_mut(x) = y;
                self.push(y)?;
            }
            Primitive::EQUAL => self.operate_comparison(|x, y| x == y)?,
            Primitive::LESS_THAN => self.operate_comparison(|x, y| x < y)?,
            Primitive::ADD => self.operate_binary(Add::add)?,
            Primitive::SUBTRACT => self.operate_binary(Sub::sub)?,
            Primitive::MULTIPLY => self.operate_binary(Mul::mul)?,
            Primitive::DIVIDE => self.operate_binary(Div::div)?,
            Primitive::GET_C => {
                let mut buffer = [0u8];

                // TODO
                // stdin().read_exact(&mut buffer)?;

                self.push(Number::new(buffer[0] as u64).into());
            }
            Primitive::PUT_C => {
                let x = self.pop()?;

                todo!();
            }
        }

        Ok(())
    }

    fn operate_binary(&mut self, operate: fn(u64, u64) -> u64) -> Result<(), Error> {
        let x = self.to_number(self.pop()?)?;
        let y = self.to_number(self.pop()?)?;

        self.push(Number::new(operate(x, y)).into())?;

        Ok(())
    }

    fn operate_comparison(&mut self, operate: fn(u64, u64) -> bool) -> Result<(), Error> {
        let x = self.to_number(self.pop()?)?;
        let y = self.to_number(self.pop()?)?;

        self.push(self.boolean(operate(x, y)))?;

        Ok(())
    }

    fn to_number(&self, value: Value) -> Result<u64, Error> {
        Ok(value.to_number().ok_or(Error::NumberExpected)?.to_u64())
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

    const HEAP_SIZE: usize = CONS_FIELD_COUNT * 16;

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

            assert_eq!(vm.pop(), Err(Error::StackUnderflow));
        }

        #[test]
        fn push_and_pop() {
            let mut vm = Vm::<HEAP_SIZE>::new();

            vm.push(Number::new(42).into()).unwrap();

            assert_eq!(vm.pop(), Ok(Number::new(42).into()));
        }

        #[test]
        fn push_and_pop_twice() {
            let mut vm = Vm::<HEAP_SIZE>::new();

            vm.push(Number::new(1).into()).unwrap();
            vm.push(Number::new(2).into()).unwrap();

            assert_eq!(vm.pop(), Ok(Number::new(2).into()));
            assert_eq!(vm.pop(), Ok(Number::new(1).into()));
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
