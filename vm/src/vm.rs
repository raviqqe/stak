use crate::{
    cons::Cons, device::Device, instruction::Instruction, number::Number, primitive::Primitive,
    value::Value, Error, Type,
};
use core::{
    fmt::{self, Display, Formatter},
    mem::replace,
    ops::{Add, Div, Mul, Sub},
};

const CONS_FIELD_COUNT: usize = 2;
const MINIMUM_HEAP_SIZE: usize = 6;
const ZERO: Number = Number::new(0);
const GC_COPIED_CAR: Cons = Cons::new(i64::MAX as u64);
const FRAME_TAG: u8 = 1;

const NIL_INDEX: u64 = 0;
const FALSE_INDEX: u64 = 1;
const TRUE_INDEX: u64 = 2;
const RIB_INDEX: u64 = 3;

macro_rules! assert_index_range {
    ($self:expr, $cons:expr) => {
        debug_assert!(
            $self.allocation_start() <= $cons.index() && $cons.index() < $self.allocation_end()
        );
    };
}

struct DecodeInput<'a> {
    codes: &'a [u8],
    index: usize,
}

#[derive(Debug)]
pub struct Vm<const N: usize, T: Device> {
    device: T,
    program_counter: Cons,
    stack: Cons,
    nil: Cons,
    allocation_index: usize,
    space: bool,
    heap: [Value; N],
}

impl<const N: usize, T: Device> Vm<N, T> {
    const SPACE_SIZE: usize = N / 2;

    pub fn new(device: T) -> Result<Self, Error> {
        if N < MINIMUM_HEAP_SIZE {
            return Err(Error::HeapSize);
        }

        let mut vm = Self {
            device,
            program_counter: Cons::new(0),
            stack: Cons::new(0),
            nil: Cons::new(0),
            allocation_index: 0,
            space: false,
            heap: [ZERO.into(); N],
        };

        let r#false = vm.allocate_raw(ZERO.into(), ZERO.into());
        let r#true = vm.allocate_raw(ZERO.into(), ZERO.into());
        vm.nil = vm.allocate_raw(r#false.into(), r#true.into());

        debug_assert!(vm.allocation_index == MINIMUM_HEAP_SIZE);

        vm.stack = vm.nil;
        vm.program_counter = vm.nil;

        Ok(vm)
    }

    pub fn run(&mut self) -> Result<(), Error> {
        while self.program_counter != self.nil {
            let instruction = Cons::try_from(self.cdr(self.program_counter))?;

            match instruction.tag() {
                Instruction::CALL => {
                    let r#return = instruction == self.nil;
                    // (code . environment)
                    let procedure = self.car(self.operand()?);
                    let stack = self.stack;
                    let argument_count = Number::try_from(self.pop()?)?;

                    match procedure {
                        // (parameter-count . instruction-list)
                        Value::Cons(code) => {
                            let parameter_count = self.car(code).try_into()?;

                            // TODO Support variadic arguments.
                            if argument_count != parameter_count {
                                return Err(Error::ArgumentCount);
                            }

                            let last_argument = self.tail(stack, parameter_count)?;

                            if r#return {
                                *self.cdr_mut(last_argument) = self.frame()?.into();
                                // Handle the case where a parameter count is zero.
                                // i.e. last_argument == stack
                                self.stack = self.cdr(stack).try_into()?;
                            } else {
                                // Reuse an argument count cons as a new frame.
                                *self.car_mut(stack) = self
                                    .allocate(
                                        self.cdr(self.program_counter),
                                        self.cdr(last_argument),
                                    )?
                                    .into();
                                *self.cdr_mut(last_argument) = stack.into();
                            }

                            // Set an environment.
                            *self.cdr_value_mut(self.cdr(last_argument))? =
                                Cons::try_from(self.cdr_value(procedure)?)?
                                    .set_tag(FRAME_TAG)
                                    .into();
                            self.program_counter = self.cdr(code).try_into()?;
                        }
                        Value::Number(primitive) => {
                            self.operate_primitive(primitive.to_u64() as u8)?;

                            if r#return {
                                let return_info = self.car(self.frame()?);

                                self.program_counter = self.car_value(return_info)?.try_into()?;
                                // Keep a value at the top of a stack.
                                *self.cdr_mut(self.stack) = self.cdr_value(return_info)?;
                            } else {
                                self.advance_program_counter()?;
                            }
                        }
                    }
                }
                Instruction::SET => {
                    let x = self.pop()?;
                    *self.car_mut(self.operand()?) = x;
                    self.advance_program_counter()?;
                }
                Instruction::GET => {
                    self.push(self.car(self.operand()?))?;
                    self.advance_program_counter()?;
                }
                Instruction::CONSTANT => {
                    self.push(self.car(self.program_counter))?;
                    self.advance_program_counter()?;
                }
                Instruction::IF => {
                    self.program_counter = (if self.pop()? == self.r#true() {
                        self.car(self.program_counter)
                    } else {
                        self.cdr(self.program_counter)
                    })
                    .try_into()?;
                }
                _ => return Err(Error::IllegalInstruction),
            }
        }

        Ok(())
    }

    fn advance_program_counter(&mut self) -> Result<(), Error> {
        self.program_counter = self.cdr(self.program_counter).try_into()?;

        Ok(())
    }

    fn operand(&self) -> Result<Cons, Error> {
        Ok(match self.car(self.program_counter) {
            Value::Cons(cons) => cons, // Direct reference to a symbol
            Value::Number(index) => self.tail(self.stack, index)?,
        })
    }

    // ((program-counter . stack) . tagged-environment)
    fn frame(&self) -> Result<Cons, Error> {
        let mut stack = self.stack;

        while Cons::try_from(self.cdr(stack))?.tag() != FRAME_TAG {
            stack = self.cdr(stack).try_into()?;
        }

        Ok(stack)
    }

    fn tail(&self, mut list: Cons, mut index: Number) -> Result<Cons, Error> {
        while index != ZERO {
            list = self.cdr(list).try_into()?;
            index = Number::new(index.to_u64() - 1);
        }

        Ok(list)
    }

    fn append(&mut self, car: Value, cdr: Cons) -> Result<Cons, Error> {
        self.allocate(car, cdr.into())
    }

    fn push(&mut self, value: Value) -> Result<(), Error> {
        self.stack = self.append(value, self.stack)?;

        Ok(())
    }

    fn pop(&mut self) -> Result<Value, Error> {
        if self.stack == self.nil {
            return Err(Error::StackUnderflow);
        }

        let value = self.car(self.stack);
        self.stack = self.cdr(self.stack).try_into()?;
        Ok(value)
    }

    fn allocate(&mut self, car: Value, cdr: Value) -> Result<Cons, Error> {
        let cons = self.allocate_raw(car, cdr);

        assert_index_range!(self, cons);

        *self.allocation_cell_mut()? = cons.into();

        if let Some(cons) = car.to_cons() {
            assert_index_range!(self, cons);
        }

        if let Some(cons) = cdr.to_cons() {
            assert_index_range!(self, cons);
        }

        if self.is_out_of_memory() {
            self.collect_garbages()?;

            if self.is_out_of_memory() {
                return Err(Error::OutOfMemory);
            }
        }

        replace(self.allocation_cell_mut()?, ZERO.into()).try_into()
    }

    fn is_out_of_memory(&self) -> bool {
        self.allocation_index >= Self::SPACE_SIZE
    }

    fn allocate_raw(&mut self, car: Value, cdr: Value) -> Cons {
        let cons = Cons::new((self.allocation_start() + self.allocation_index) as u64);

        assert_index_range!(self, cons);

        *self.car_mut(cons) = car;
        *self.cdr_mut(cons) = cdr;

        self.allocation_index += CONS_FIELD_COUNT;

        debug_assert!(self.allocation_index <= Self::SPACE_SIZE);

        cons
    }

    fn allocation_start(&self) -> usize {
        if self.space {
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
        Ok(self.car(cons.try_into()?))
    }

    fn cdr_value(&self, cons: Value) -> Result<Value, Error> {
        Ok(self.cdr(cons.try_into()?))
    }

    fn car_mut(&mut self, cons: Cons) -> &mut Value {
        &mut self.heap[cons.index()]
    }

    fn cdr_mut(&mut self, cons: Cons) -> &mut Value {
        &mut self.heap[cons.index() + 1]
    }

    fn car_value_mut(&mut self, cons: Value) -> Result<&mut Value, Error> {
        Ok(self.car_mut(cons.try_into()?))
    }

    fn cdr_value_mut(&mut self, cons: Value) -> Result<&mut Value, Error> {
        Ok(self.cdr_mut(cons.try_into()?))
    }

    fn boolean(&self, value: bool) -> Value {
        if value {
            self.cdr(self.nil)
        } else {
            self.car(self.nil)
        }
    }

    fn r#false(&self) -> Value {
        self.boolean(false)
    }

    fn r#true(&self) -> Value {
        self.boolean(true)
    }

    // Primitive operations

    fn operate_primitive(&mut self, primitive: u8) -> Result<(), Error> {
        match primitive {
            Primitive::RIB => {
                let [car, cdr, tag] = self.pop_arguments::<3>()?;
                let rib = self.allocate(
                    car,
                    Cons::try_from(cdr)?
                        .set_tag(Number::try_from(tag)?.to_u64() as u8)
                        .into(),
                )?;
                self.push(rib.into())?;
            }
            Primitive::CONS => {
                let [car, cdr] = self.pop_arguments::<2>()?;
                let cons = self.allocate(car, cdr)?;
                self.push(cons.into())?;
            }
            Primitive::ID => {}
            Primitive::POP => {
                self.pop()?;
            }
            Primitive::SKIP => {
                let [x, _] = self.pop_arguments::<2>()?;
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
                let [x, y] = self.pop_arguments::<2>()?;
                *self.car_value_mut(x)? = y;
                self.push(y)?;
            }
            Primitive::SET_CDR => {
                let [x, y] = self.pop_arguments::<2>()?;
                *self.cdr_value_mut(x)? = y;
                self.push(y)?;
            }
            Primitive::EQUAL => self.operate_comparison(|x, y| x == y)?,
            Primitive::LESS_THAN => self.operate_comparison(|x, y| x < y)?,
            Primitive::ADD => self.operate_binary(Add::add)?,
            Primitive::SUBTRACT => self.operate_binary(Sub::sub)?,
            Primitive::MULTIPLY => self.operate_binary(Mul::mul)?,
            Primitive::DIVIDE => self.operate_binary(Div::div)?,
            Primitive::READ => {
                let byte = self.device.read().map_err(|_| Error::ReadInput)?;
                self.push(Number::new(byte as u64).into())?;
            }
            Primitive::WRITE => {
                let byte = self.pop()?;
                self.device
                    .write(Number::try_from(byte)?.to_u64() as u8)
                    .map_err(|_| Error::ReadInput)?;
            }
            _ => return Err(Error::IllegalPrimitive),
        }

        Ok(())
    }

    fn operate_binary(&mut self, operate: fn(u64, u64) -> u64) -> Result<(), Error> {
        let [x, y] = self.pop_number_arguments::<2>()?;

        self.push(Number::new(operate(x.to_u64(), y.to_u64())).into())?;

        Ok(())
    }

    fn operate_comparison(&mut self, operate: fn(u64, u64) -> bool) -> Result<(), Error> {
        let [x, y] = self.pop_number_arguments::<2>()?;

        self.push(self.boolean(operate(x.to_u64(), y.to_u64())))?;

        Ok(())
    }

    fn pop_number_arguments<const M: usize>(&mut self) -> Result<[Number; M], Error> {
        let mut numbers = [ZERO; M];

        for (index, value) in self.pop_arguments::<M>()?.into_iter().enumerate() {
            numbers[index] = Number::try_from(value)?;
        }

        Ok(numbers)
    }

    fn pop_arguments<const M: usize>(&mut self) -> Result<[Value; M], Error> {
        let mut values = [ZERO.into(); M];

        for index in 0..M {
            values[M - 1 - index] = self.pop()?;
        }

        Ok(values)
    }

    // Garbage collection

    fn collect_garbages(&mut self) -> Result<(), Error> {
        self.allocation_index = 0;
        self.space = !self.space;

        self.program_counter = self.copy_cons(self.program_counter)?;
        self.stack = self.copy_cons(self.stack)?;
        self.nil = self.copy_cons(self.nil)?;

        for index in self.allocation_start()..self.allocation_end() {
            self.heap[index] = self.copy_value(self.heap[index])?;
        }

        Ok(())
    }

    fn copy_value(&mut self, value: Value) -> Result<Value, Error> {
        Ok(if let Some(cons) = value.to_cons() {
            self.copy_cons(cons)?.into()
        } else {
            value
        })
    }

    fn copy_cons(&mut self, cons: Cons) -> Result<Cons, Error> {
        Ok(if self.car(cons) == GC_COPIED_CAR.into() {
            // Get a forward pointer.
            self.cdr(cons).try_into()?
        } else {
            let copy = self.allocate_raw(self.car(cons), self.cdr(cons));

            *self.car_mut(cons) = GC_COPIED_CAR.into();
            // Set a forward pointer.
            *self.cdr_mut(cons) = copy.into();

            copy
        }
        .set_tag(cons.tag()))
    }

    // Input decoding

    pub fn initialize(&mut self, codes: &[u8]) -> Result<(), Error> {
        self.program_counter = self.nil;
        self.stack = self.nil;

        let mut input = DecodeInput { codes, index: 0 };

        // Initialize a rib primitive under a root.
        *self.rib_mut()? = self
            .allocate(
                ZERO.into(),
                Cons::new(0).set_tag(Type::Procedure as u8).into(),
            )?
            .into();

        self.decode_symbols(&mut input)?;
        self.decode_instructions(&mut input)?;

        // Implicit top-level frame
        let return_info = self.allocate(self.nil.into(), self.nil.into())?.into();
        self.stack = self.allocate(return_info, self.nil.set_tag(FRAME_TAG).into())?;

        // Remove references to symbols and a rib primitive.
        *self.symbols_mut()? = ZERO.into();
        *self.rib_mut()? = ZERO.into();

        Ok(())
    }

    fn symbols(&self) -> Result<Cons, Error> {
        self.car_value(self.r#false())?.try_into()
    }

    fn symbols_mut(&mut self) -> Result<&mut Value, Error> {
        self.car_value_mut(self.r#false())
    }

    fn rib(&self) -> Result<Cons, Error> {
        self.cdr_value(self.r#false())?.try_into()
    }

    fn rib_mut(&mut self) -> Result<&mut Value, Error> {
        self.cdr_value_mut(self.r#false())
    }

    fn allocation_cell_mut(&mut self) -> Result<&mut Value, Error> {
        self.cdr_value_mut(self.r#true())
    }

    fn decode_symbols(&mut self, input: &mut DecodeInput) -> Result<(), Error> {
        let mut symbol = self.nil;

        loop {
            match Self::decode_byte(input).ok_or(Error::EndOfInput)? {
                character @ (b',' | b';') => {
                    self.push(symbol.into())?;

                    if character == b';' {
                        break;
                    }
                }
                character => symbol = self.append(Number::new(character as u64).into(), symbol)?,
            }
        }

        *self.symbols_mut()? = self.stack.into();
        self.stack = self.nil;

        Ok(())
    }

    fn decode_instructions(&mut self, input: &mut DecodeInput) -> Result<(), Error> {
        while let Some(instruction) = Self::decode_byte(input) {
            let (car, tag) = match instruction {
                code::Instruction::RETURN_CALL => {
                    self.push(self.program_counter.into())?;
                    self.program_counter = self.nil;

                    (self.decode_operand(input)?, Instruction::CALL)
                }
                code::Instruction::CALL => (self.decode_operand(input)?, Instruction::CALL),
                code::Instruction::SET => (self.decode_operand(input)?, Instruction::SET),
                code::Instruction::GET => (self.decode_operand(input)?, Instruction::GET),
                code::Instruction::CONSTANT => (
                    Number::new(Self::decode_integer(input).ok_or(Error::MissingOperand)?).into(),
                    Instruction::CONSTANT,
                ),
                code::Instruction::IF => {
                    let then = self.program_counter;
                    self.program_counter = self.pop()?.try_into()?;

                    (then.into(), Instruction::IF)
                }
                _ => return Err(Error::IllegalInstruction),
            };

            self.program_counter = self.append(car, self.program_counter.set_tag(tag))?;
        }

        self.stack = self.nil;

        Ok(())
    }

    fn decode_operand(&self, input: &mut DecodeInput) -> Result<Value, Error> {
        let integer = Self::decode_integer(input).ok_or(Error::MissingOperand)?;
        let index = Number::new(integer >> 1);

        Ok(if integer & 1 == 0 {
            match index.to_u64() {
                NIL_INDEX => self.nil.into(),
                FALSE_INDEX => self.r#false(),
                TRUE_INDEX => self.r#true(),
                RIB_INDEX => self.rib()?.into(),
                _ => self.car(self.tail(self.symbols()?, index)?),
            }
        } else {
            index.into()
        })
    }

    fn decode_integer(input: &mut DecodeInput) -> Option<u64> {
        let mut y = 0;

        while {
            y *= code::INTEGER_BASE;
            let x = Self::decode_byte(input)? as i8;

            y += (if x < 0 { -1 } else { 1 } * x) as u64;

            x < 0
        } {}

        Some(y)
    }

    fn decode_byte(input: &mut DecodeInput) -> Option<u8> {
        if input.index >= input.codes.len() {
            return None;
        }

        let byte = input.codes[input.codes.len() - 1 - input.index];
        input.index += 1;
        Some(byte)
    }
}

impl<T: Device, const N: usize> Display for Vm<N, T> {
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
    use crate::FixedBufferDevice;
    use std::format;

    const HEAP_SIZE: usize = 2 * 16;

    type FakeDevice = FixedBufferDevice<16, 16>;

    fn create_vm() -> Vm<HEAP_SIZE, FakeDevice> {
        Vm::<HEAP_SIZE, _>::new(FakeDevice::new()).unwrap()
    }

    #[test]
    fn create() {
        let vm = create_vm();

        insta::assert_display_snapshot!(vm);
    }

    #[test]
    fn run_nothing() {
        let mut vm = create_vm();

        vm.run().unwrap();

        insta::assert_display_snapshot!(vm);
    }

    #[test]
    fn run_nothing_after_garbage_collection() {
        let mut vm = create_vm();

        vm.collect_garbages().unwrap();
        vm.run().unwrap();

        insta::assert_display_snapshot!(vm);
    }

    #[test]
    fn create_list() {
        let mut vm = create_vm();

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
            let mut vm = create_vm();

            assert_eq!(vm.pop(), Err(Error::StackUnderflow));
        }

        #[test]
        fn push_and_pop() {
            let mut vm = create_vm();

            vm.push(Number::new(42).into()).unwrap();

            assert_eq!(vm.pop(), Ok(Number::new(42).into()));
        }

        #[test]
        fn push_and_pop_twice() {
            let mut vm = create_vm();

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
            let mut vm = create_vm();

            vm.allocate(ZERO.into(), ZERO.into()).unwrap();
            vm.collect_garbages().unwrap();

            insta::assert_display_snapshot!(vm);
        }

        #[test]
        fn collect_stack() {
            let mut vm = create_vm();

            vm.push(Number::new(42).into()).unwrap();
            vm.collect_garbages().unwrap();

            insta::assert_display_snapshot!(vm);
        }

        #[test]
        fn collect_deep_stack() {
            let mut vm = create_vm();

            vm.push(Number::new(1).into()).unwrap();
            vm.push(Number::new(2).into()).unwrap();
            vm.collect_garbages().unwrap();

            insta::assert_display_snapshot!(vm);
        }

        #[test]
        fn collect_cycle() {
            let mut vm = create_vm();

            let cons = vm.allocate(ZERO.into(), ZERO.into()).unwrap();
            *vm.cdr_mut(cons) = cons.into();

            vm.collect_garbages().unwrap();

            insta::assert_display_snapshot!(vm);
        }
    }

    mod instruction {
        use super::*;
        use alloc::vec;
        use code::{encode, Instruction, Operand, Program};

        fn run_program(program: &Program) {
            let mut vm = create_vm();

            vm.initialize(&encode(program)).unwrap();

            vm.run().unwrap()
        }

        #[test]
        fn run_nothing() {
            run_program(&Program::new(vec![], vec![]));
        }

        #[test]
        fn constant() {
            run_program(&Program::new(vec![], vec![Instruction::Constant(42)]));
        }

        #[test]
        fn set_global() {
            run_program(&Program::new(
                vec!["x".into()],
                vec![
                    Instruction::Constant(42),
                    Instruction::Set(Operand::Global(0)),
                ],
            ));
        }

        #[test]
        fn set_local() {
            run_program(&Program::new(
                vec![],
                vec![
                    Instruction::Constant(0),
                    Instruction::Constant(42),
                    Instruction::Set(Operand::Local(0)),
                ],
            ));
        }

        #[test]
        fn get_global() {
            run_program(&Program::new(
                vec!["x".into()],
                vec![Instruction::Get(Operand::Global(0))],
            ));
        }

        #[test]
        fn get_local() {
            run_program(&Program::new(
                vec![],
                vec![
                    Instruction::Constant(42),
                    Instruction::Get(Operand::Local(0)),
                ],
            ));
        }

        #[test]
        fn r#if() {
            run_program(&Program::new(
                vec!["f".into()],
                vec![
                    Instruction::Constant(0),
                    Instruction::Get(Operand::Global(NIL_INDEX)),
                    Instruction::Constant(0),
                    Instruction::Constant(3),
                    Instruction::Get(Operand::Global(FALSE_INDEX)),
                    Instruction::If(
                        vec![Instruction::Call(Operand::Global(RIB_INDEX), true)],
                        vec![Instruction::Call(Operand::Global(RIB_INDEX), true)],
                    ),
                ],
            ));
        }
    }
}
