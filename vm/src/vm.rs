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
    program_counter: Cons,
    stack: Cons,
    nil: Cons,
    allocation_index: usize,
    gc_inverse: bool,
}

impl<const N: usize> Vm<N> {
    const SPACE_SIZE: usize = N / 2;

    pub fn new() -> Result<Self, Error> {
        let mut vm = Self {
            heap: [ZERO.into(); N],
            program_counter: Cons::new(0),
            stack: Cons::new(0),
            nil: Cons::new(0),
            allocation_index: 0,
            gc_inverse: false,
        };

        vm.initialize()?;

        Ok(vm)
    }

    fn initialize(&mut self) -> Result<(), Error> {
        let r#false = self.allocate(ZERO.into(), ZERO.into())?;
        let r#true = self.allocate(ZERO.into(), ZERO.into())?;

        self.nil = self.allocate(r#false.into(), r#true.into())?;
        self.stack = self.nil;

        Ok(())
    }

    pub fn run(&mut self) -> Result<(), Error> {
        loop {
            let instruction = self.get_car(self.program_counter);
            #[cfg(feature = "trace")]
            println!("instruction: {}", instruction.to_raw());

            match instruction.to_raw() {
                Instruction::HALT => return Ok(()),
                Instruction::APPLY => {
                    let jump = self.get_tag(self.program_counter) == ZERO;
                    let procedure = self.get_procedure();
                    let code = self.get_code();
                    let mut argument_count = self.pop();

                    if !code.is_rib() {
                        self.operate_primitive(
                            Primitive::try_from(code.to_raw())
                                .map_err(|_| Error::IllegalPrimitive)?,
                        );

                        if jump {
                            self.program_counter = self.get_continuation();
                            *self.get_cdr_mut(self.stack) = self.get_car(self.program_counter);
                        }

                        self.advance_program_counter();
                    } else {
                        debug_assert!(!self.get_car(code).is_rib());
                        debug_assert!(!argument_count.is_rib());

                        let parameter_info = self.get_car(code).to_raw();
                        let parameter_count = Object::Number(parameter_info >> 1);
                        let variadic = parameter_info & 1 != 0;

                        let mut stack = self.allocate_rib(ZERO, procedure, PAIR_TAG);
                        *self.get_car_mut(self.program_counter) = code;

                        if (!variadic && parameter_count != argument_count)
                            || (variadic && parameter_count.to_raw() > argument_count.to_raw())
                        {
                            return Err(Error::ArgumentCount);
                        }

                        argument_count =
                            Object::Number(argument_count.to_raw() - parameter_count.to_raw());

                        if variadic {
                            todo!("{}", argument_count.to_raw());
                        }

                        for _ in 0..parameter_count.to_raw() {
                            let argument = self.pop();
                            stack = self.allocate_rib(argument, stack, PAIR_TAG);
                        }

                        let c2 = self.get_list_tail(
                            stack,
                            Object::Number(parameter_count.to_raw() + if variadic { 1 } else { 0 }),
                        );

                        if jump {
                            let continuation = self.get_continuation();
                            *self.get_car_mut(c2) = self.get_car(continuation);
                            *self.get_tag_mut(c2) = self.get_tag(continuation);
                        } else {
                            *self.get_car_mut(c2) = self.stack;
                            *self.get_tag_mut(c2) = self.get_tag(self.program_counter);
                        }

                        self.stack = stack;

                        let next_counter = self.get_car(self.program_counter);
                        *self.get_car_mut(self.program_counter) = instruction;
                        self.program_counter = self.get_tag(next_counter);
                    }
                }
                Instruction::SET => {
                    let x = self.pop();

                    let rib = if !self.get_cdr(self.program_counter).is_rib() {
                        self.get_list_tail(self.stack, self.get_cdr(self.program_counter))
                    } else {
                        self.get_cdr(self.program_counter)
                    };

                    *self.get_car_mut(rib) = x;

                    self.advance_program_counter();
                }
                Instruction::GET => {
                    self.push(
                        self.get_operand(self.get_cdr(self.program_counter)),
                        PAIR_TAG,
                    );
                    self.advance_program_counter();
                }
                Instruction::CONSTANT => {
                    self.push(self.get_cdr(self.program_counter), PAIR_TAG);
                    self.advance_program_counter();
                }
                Instruction::IF => {
                    self.program_counter = if self.pop().to_raw() != self.r#false.to_raw() {
                        self.get_cdr(self.program_counter)
                    } else {
                        self.get_tag(self.program_counter)
                    };
                }
                _ => return Err(Error::IllegalInstruction),
            }
        }
    }

    fn append(&mut self, car: Value, cdr: Cons) -> Result<Cons, Error> {
        self.allocate(car, cdr.into())
    }

    pub fn push(&mut self, value: Value) -> Result<(), Error> {
        self.stack = self.append(value, self.stack)?;

        Ok(())
    }

    pub fn pop(&mut self) -> Result<Value, Error> {
        if self.stack == self.nil {
            return Err(Error::StackUnderflow);
        }

        let value = self.car(self.stack);
        self.stack = Self::to_cons(self.cdr(self.stack))?;
        Ok(value)
    }

    pub fn allocate(&mut self, car: Value, cdr: Value) -> Result<Cons, Error> {
        let cons = self.allocate_raw(car, cdr);

        debug_assert!(self.allocation_index <= Self::SPACE_SIZE);

        if self.allocation_index == Self::SPACE_SIZE {
            self.collect_garbages()?;

            if self.allocation_index == Self::SPACE_SIZE {
                return Err(Error::OutOfMemory);
            }
        }

        Ok(cons)
    }

    fn allocate_raw(&mut self, car: Value, cdr: Value) -> Cons {
        let cons = Cons::new((self.allocation_start() + self.allocation_index) as u64);

        *self.car_mut(cons) = car;
        *self.cdr_mut(cons) = cdr;

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

    fn car_value(&self, cons: Value) -> Result<Value, Error> {
        Ok(self.car(Self::to_cons(cons)?))
    }

    fn cdr_value(&self, cons: Value) -> Result<Value, Error> {
        Ok(self.cdr(Self::to_cons(cons)?))
    }

    fn car_mut(&mut self, cons: Cons) -> &mut Value {
        &mut self.heap[cons.index()]
    }

    fn cdr_mut(&mut self, cons: Cons) -> &mut Value {
        &mut self.heap[cons.index() + 1]
    }

    fn car_value_mut(&mut self, cons: Value) -> Result<&mut Value, Error> {
        Ok(&mut self.heap[Self::to_cons(cons)?.index()])
    }

    fn cdr_value_mut(&mut self, cons: Value) -> Result<&mut Value, Error> {
        Ok(&mut self.heap[Self::to_cons(cons)?.index() + 1])
    }

    fn boolean(&self, value: bool) -> Value {
        if value {
            self.cdr(self.nil)
        } else {
            self.car(self.nil)
        }
    }

    fn to_cons(value: Value) -> Result<Cons, Error> {
        value.to_cons().ok_or(Error::ConsExpected)
    }

    fn to_u64(value: Value) -> Result<u64, Error> {
        Ok(value.to_number().ok_or(Error::NumberExpected)?.to_u64())
    }

    // Primitive operations

    pub fn operate_primitive(&mut self, primitive: u8) -> Result<(), Error> {
        match primitive {
            Primitive::CONS => {
                let car = self.pop()?;
                let cdr = self.pop()?;
                let cons = self.allocate(car, cdr)?;
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
                let car = self.pop()?;
                let cons = self.allocate(car, self.stack.into())?;

                self.push(cons.into())?;
            }
            Primitive::IS_CONS => {
                let x = self.pop()?;
                self.push(self.boolean(x.is_cons()))?;
            }
            Primitive::CAR => {
                let x = self.pop()?;
                self.push(self.car_value(x)?)?;
            }
            Primitive::CDR => {
                let x = self.pop()?;
                self.push(self.cdr_value(x)?)?;
            }
            Primitive::SET_CAR => {
                let x = self.pop()?;
                let y = self.pop()?;
                *self.car_value_mut(x)? = y;
                self.push(y)?;
            }
            Primitive::SET_CDR => {
                let x = self.pop()?;
                let y = self.pop()?;
                *self.cdr_value_mut(x)? = y;
                self.push(y)?;
            }
            Primitive::EQUAL => self.operate_comparison(|x, y| x == y)?,
            Primitive::LESS_THAN => self.operate_comparison(|x, y| x < y)?,
            Primitive::ADD => self.operate_binary(Add::add)?,
            Primitive::SUBTRACT => self.operate_binary(Sub::sub)?,
            Primitive::MULTIPLY => self.operate_binary(Mul::mul)?,
            Primitive::DIVIDE => self.operate_binary(Div::div)?,
            Primitive::GET_C => {
                let buffer = [0u8];

                // TODO
                // stdin().read_exact(&mut buffer)?;

                self.push(Number::new(buffer[0] as u64).into())?;
            }
            Primitive::PUT_C => {
                let _x = self.pop()?;

                todo!();
            }
            _ => return Err(Error::IllegalPrimitive),
        }

        Ok(())
    }

    fn operate_binary(&mut self, operate: fn(u64, u64) -> u64) -> Result<(), Error> {
        let x = Self::to_u64(self.pop()?)?;
        let y = Self::to_u64(self.pop()?)?;

        self.push(Number::new(operate(x, y)).into())?;

        Ok(())
    }

    fn operate_comparison(&mut self, operate: fn(u64, u64) -> bool) -> Result<(), Error> {
        let x = Self::to_u64(self.pop()?)?;
        let y = Self::to_u64(self.pop()?)?;

        self.push(self.boolean(operate(x, y)))?;

        Ok(())
    }

    // Garbage collection

    fn collect_garbages(&mut self) -> Result<(), Error> {
        self.allocation_index = 0;
        self.gc_inverse = !self.gc_inverse;

        self.stack = Self::to_cons(self.copy_value(self.stack.into()))?;

        for index in self.allocation_start()..self.allocation_end() {
            self.heap[index] = self.copy_value(self.heap[index]);
        }

        Ok(())
    }

    fn copy_value(&mut self, value: Value) -> Value {
        if let Some(cons) = value.to_cons() {
            if self.car(cons) == GC_COPIED_CAR.into() {
                // Get a forward pointer.
                self.cdr(cons)
            } else {
                let copy = self.allocate_raw(self.car(cons), self.cdr(cons));

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
        let vm = Vm::<HEAP_SIZE>::new().unwrap();

        insta::assert_display_snapshot!(vm);
    }

    #[test]
    fn run_nothing() {
        let mut vm = Vm::<HEAP_SIZE>::new().unwrap();

        vm.run().unwrap();

        insta::assert_display_snapshot!(vm);
    }

    #[test]
    fn create_list() {
        let mut vm = Vm::<HEAP_SIZE>::new().unwrap();

        let list = vm.append(Number::new(1).into(), vm.nil).unwrap();

        insta::assert_display_snapshot!(vm);

        let list = vm.append(Number::new(2).into(), list).unwrap();

        insta::assert_display_snapshot!(vm);

        vm.append(Number::new(3).into(), list).unwrap();

        insta::assert_display_snapshot!(vm);
    }

    mod stack {
        use super::*;

        #[test]
        fn pop_nothing() {
            let mut vm = Vm::<HEAP_SIZE>::new().unwrap();

            assert_eq!(vm.pop(), Err(Error::StackUnderflow));
        }

        #[test]
        fn push_and_pop() {
            let mut vm = Vm::<HEAP_SIZE>::new().unwrap();

            vm.push(Number::new(42).into()).unwrap();

            assert_eq!(vm.pop(), Ok(Number::new(42).into()));
        }

        #[test]
        fn push_and_pop_twice() {
            let mut vm = Vm::<HEAP_SIZE>::new().unwrap();

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
            let mut vm = Vm::<HEAP_SIZE>::new().unwrap();

            vm.allocate(ZERO.into(), ZERO.into()).unwrap();
            vm.collect_garbages().unwrap();

            insta::assert_display_snapshot!(vm);
        }

        #[test]
        fn collect_stack() {
            let mut vm = Vm::<HEAP_SIZE>::new().unwrap();

            vm.push(Number::new(42).into()).unwrap();
            vm.collect_garbages().unwrap();

            insta::assert_display_snapshot!(vm);
        }

        #[test]
        fn collect_deep_stack() {
            let mut vm = Vm::<HEAP_SIZE>::new().unwrap();

            vm.push(Number::new(1).into()).unwrap();
            vm.push(Number::new(2).into()).unwrap();
            vm.collect_garbages().unwrap();

            insta::assert_display_snapshot!(vm);
        }

        #[test]
        fn collect_cycle() {
            let mut vm = Vm::<HEAP_SIZE>::new().unwrap();

            let cons = vm.allocate(ZERO.into(), ZERO.into()).unwrap();
            *vm.cdr_mut(cons) = cons.into();

            vm.collect_garbages().unwrap();

            insta::assert_display_snapshot!(vm);
        }
    }
}
