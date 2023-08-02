use crate::{
    cons::{Cons, FALSE, MOVED, NULL, TRUE},
    instruction::Instruction,
    number::Number,
    primitive::Primitive,
    r#type::Type,
    value::{TypedValue, Value},
    Error,
};
use core::{
    fmt::{self, Display, Formatter},
    ops::{Add, Div, Mul, Sub},
};
use device::Device;

const CONS_FIELD_COUNT: usize = 2;
const ZERO: Number = Number::new(0);
const FRAME_TAG: u8 = 1;

const SINGLETONS: &[Cons] = &[FALSE, TRUE, NULL];

macro_rules! trace {
    ($prefix:literal, $data:expr) => {
        #[cfg(feature = "trace")]
        {
            std::eprint!($prefix);
            std::eprintln!(": {}", $data);
        }
    };
}

macro_rules! trace_heap {
    ($self:expr) => {
        #[cfg(feature = "trace_heap")]
        std::eprintln!("{}", $self);
    };
}

macro_rules! assert_index_range {
    ($self:expr, $cons:expr) => {
        debug_assert!(
            SINGLETONS.contains(&$cons)
                || $self.allocation_start() <= $cons.index()
                    && $cons.index() < $self.allocation_end()
        );
    };
}

#[derive(Debug)]
pub struct Vm<const N: usize, T: Device> {
    device: T,
    program_counter: Cons,
    stack: Cons,
    symbols: Cons,
    cons: Cons,
    allocation_index: usize,
    space: bool,
    heap: [Value; N],
}

impl<const N: usize, T: Device> Vm<N, T> {
    const SPACE_SIZE: usize = N / 2;

    pub fn new(device: T) -> Result<Self, Error> {
        let mut vm = Self {
            device,
            program_counter: NULL,
            stack: NULL,
            symbols: NULL,
            cons: NULL,
            allocation_index: 0,
            space: false,
            heap: [ZERO.into(); N],
        };

        vm.initialize_cons()?;

        Ok(vm)
    }

    pub fn run(&mut self) -> Result<(), Error> {
        while self.program_counter != NULL {
            let instruction = Cons::try_from(self.cdr(self.program_counter))?;

            trace!("instruction", instruction.tag());

            match instruction.tag() {
                Instruction::CALL => {
                    let r#return = instruction == NULL;
                    let procedure = self.procedure()?;
                    let environment = Cons::try_from(self.cdr(procedure))?;

                    trace!("procedure", procedure);
                    trace!("return", r#return);

                    if environment.tag() != Type::Procedure as u8 {
                        return Err(Error::ProcedureExpected);
                    }

                    match self.code(procedure).to_typed() {
                        TypedValue::Cons(code) => {
                            let argument_count = Number::try_from(self.car(self.stack))?;
                            let parameter_count = self.car(code).try_into()?;

                            trace!("argument count", argument_count);
                            trace!("parameter count", parameter_count);

                            // TODO Support variadic arguments.
                            if argument_count != parameter_count {
                                return Err(Error::ArgumentCount);
                            }

                            let last_argument = self.tail(self.stack, parameter_count)?;

                            let frame = if r#return {
                                self.frame()?
                            } else {
                                // Reuse an argument count cons as a new frame.
                                *self.car_mut(self.cons) = self.cdr(self.program_counter);
                                *self.cdr_mut(self.cons) = self.cdr(last_argument);
                                *self.car_mut(self.stack) = self.cons.into();
                                self.stack
                            };
                            *self.cdr_mut(last_argument) = frame.into();

                            // Drop an argument count.
                            self.pop()?;

                            *self.cdr_mut(frame) = environment.set_tag(FRAME_TAG).into();
                            self.program_counter = self.cdr(code).try_into()?;

                            if !r#return {
                                self.initialize_cons()?;
                            }
                        }
                        TypedValue::Number(primitive) => {
                            // Drop an argument count.
                            self.pop()?;
                            self.operate_primitive(primitive.to_i64() as u8)?;

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
                    let operand = self.operand()?;

                    trace!("operand", operand);

                    let value = self.car(operand);

                    trace!("value", value);

                    self.push(value)?;
                    self.advance_program_counter()?;
                }
                Instruction::CONSTANT => {
                    let constant = self.car(self.program_counter);

                    trace!("constant", constant);

                    self.push(constant)?;
                    self.advance_program_counter()?;
                }
                Instruction::IF => {
                    self.program_counter = (if self.pop()? == FALSE.into() {
                        self.cdr(self.program_counter)
                    } else {
                        self.car(self.program_counter)
                    })
                    .try_into()?;
                }
                _ => return Err(Error::IllegalInstruction),
            }

            trace_heap!(self);

            #[cfg(feature = "gc_always")]
            self.collect_garbages(None)?;
        }

        Ok(())
    }

    fn advance_program_counter(&mut self) -> Result<(), Error> {
        self.program_counter = self.cdr(self.program_counter).try_into()?;

        Ok(())
    }

    fn operand(&self) -> Result<Cons, Error> {
        Ok(match self.car(self.program_counter).to_typed() {
            TypedValue::Cons(cons) => cons, // Direct reference to a symbol
            TypedValue::Number(index) => self.tail(self.stack, index)?,
        })
    }

    // (code . environment)
    fn procedure(&self) -> Result<Cons, Error> {
        self.car(self.operand()?).try_into()
    }

    // (parameter-count . instruction-list) | primitive
    fn code(&self, procedure: Cons) -> Value {
        self.car(procedure)
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
            index = Number::new(index.to_i64() - 1);
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
        if self.stack == NULL {
            return Err(Error::StackUnderflow);
        }

        let value = self.car(self.stack);
        self.stack = self.cdr(self.stack).try_into()?;
        Ok(value)
    }

    fn allocate(&mut self, car: Value, cdr: Value) -> Result<Cons, Error> {
        let mut cons = self.allocate_raw(car, cdr)?;

        assert_index_range!(self, cons);

        if let Some(cons) = car.to_cons() {
            assert_index_range!(self, cons);
        }

        if let Some(cons) = cdr.to_cons() {
            assert_index_range!(self, cons);
        }

        if self.is_out_of_memory() {
            self.collect_garbages(Some(&mut cons))?;
        }

        Ok(cons)
    }

    fn allocate_raw(&mut self, car: Value, cdr: Value) -> Result<Cons, Error> {
        if self.is_out_of_memory() {
            return Err(Error::OutOfMemory);
        }

        let cons = Cons::new(self.allocation_end() as u64);
        self.allocation_index += CONS_FIELD_COUNT;

        assert_index_range!(self, cons);

        *self.car_mut(cons) = car;
        *self.cdr_mut(cons) = cdr;

        debug_assert!(self.allocation_index <= Self::SPACE_SIZE);

        Ok(cons)
    }

    fn initialize_cons(&mut self) -> Result<(), Error> {
        self.cons = self.allocate(FALSE.into(), FALSE.into())?;

        Ok(())
    }

    fn is_out_of_memory(&self) -> bool {
        self.allocation_index >= Self::SPACE_SIZE
    }

    fn allocation_start(&self) -> usize {
        if self.space {
            Self::SPACE_SIZE
        } else {
            0
        }
    }

    fn allocation_end(&self) -> usize {
        self.allocation_start() + self.allocation_index
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
        if value { TRUE } else { FALSE }.into()
    }

    // Primitive operations

    fn operate_primitive(&mut self, primitive: u8) -> Result<(), Error> {
        trace!("primitive", primitive);

        match primitive {
            Primitive::RIB => {
                let [car, cdr, tag] = self.pop_arguments::<3>()?;
                let rib = self.allocate(
                    car,
                    Cons::try_from(cdr)?
                        .set_tag(Number::try_from(tag)?.to_i64() as u8)
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
                // TODO This shouldn't be a primitive as it cannot be a function in a Stak VM at
                // least... Can we always use a skip primitive? Should this be an instruction?
                self.pop()?;
            }
            Primitive::SKIP => {
                let [_, x] = self.pop_arguments::<2>()?;
                self.push(x)?;
            }
            Primitive::CLOSE => {
                let procedure = self.pop()?;
                let cons = self.allocate(
                    self.car_value(procedure)?,
                    self.stack.set_tag(Type::Procedure as u8).into(),
                )?;

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
            Primitive::TAG => {
                let x = self.pop()?;
                self.push(Number::new(Cons::try_from(self.cdr_value(x)?)?.tag() as i64).into())?;
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
            Primitive::SET_TAG => {
                let [x, y] = self.pop_arguments::<2>()?;
                *self.cdr_value_mut(x)? = Cons::try_from(self.cdr_value(x)?)?
                    .set_tag(Number::try_from(y)?.to_i64() as u8)
                    .into();
                self.push(y)?;
            }
            Primitive::EQUAL => {
                let [x, y] = self.pop_arguments::<2>()?;
                self.push(self.boolean(x == y))?;
            }
            Primitive::LESS_THAN => self.operate_comparison(|x, y| x < y)?,
            Primitive::ADD => self.operate_binary(Add::add)?,
            Primitive::SUBTRACT => self.operate_binary(Sub::sub)?,
            Primitive::MULTIPLY => self.operate_binary(Mul::mul)?,
            Primitive::DIVIDE => self.operate_binary(Div::div)?,
            Primitive::READ => {
                let byte = self.device.read().map_err(|_| Error::ReadInput)?;
                self.push(Number::new(byte as i64).into())?;
            }
            Primitive::WRITE => {
                let byte = self.pop()?;
                self.device
                    .write(Number::try_from(byte)?.to_i64() as u8)
                    .map_err(|_| Error::WriteOutput)?;
                self.push(byte)?;
            }
            _ => return Err(Error::IllegalPrimitive),
        }

        Ok(())
    }

    fn operate_binary(&mut self, operate: fn(i64, i64) -> i64) -> Result<(), Error> {
        let [x, y] = self.pop_number_arguments::<2>()?;

        self.push(Number::new(operate(x.to_i64(), y.to_i64())).into())?;

        Ok(())
    }

    fn operate_comparison(&mut self, operate: fn(i64, i64) -> bool) -> Result<(), Error> {
        let [x, y] = self.pop_number_arguments::<2>()?;

        self.push(self.boolean(operate(x.to_i64(), y.to_i64())))?;

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

    fn collect_garbages(&mut self, cons: Option<&mut Cons>) -> Result<(), Error> {
        self.allocation_index = 0;
        self.space = !self.space;

        self.program_counter = self.copy_cons(self.program_counter)?;
        self.stack = self.copy_cons(self.stack)?;
        self.symbols = self.copy_cons(self.symbols)?;
        self.cons = self.copy_cons(self.cons)?;

        if let Some(cons) = cons {
            *cons = self.copy_cons(*cons)?;
        }

        let mut index = self.allocation_start();

        while index < self.allocation_end() {
            self.heap[index] = self.copy_value(self.heap[index])?;
            index += 1;
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
        Ok(if SINGLETONS.contains(&cons) {
            cons
        } else if self.car(cons) == MOVED.into() {
            // Get a forward pointer.
            self.cdr(cons).try_into()?
        } else {
            let copy = self.allocate_raw(self.car(cons), self.cdr(cons))?;

            *self.car_mut(cons) = MOVED.into();
            // Set a forward pointer.
            *self.cdr_mut(cons) = copy.into();

            copy
        }
        .set_tag(cons.tag()))
    }

    // Input decoding

    pub fn initialize(&mut self, input: impl IntoIterator<Item = u8>) -> Result<(), Error> {
        let mut input = input.into_iter();

        self.program_counter = NULL;
        self.stack = NULL;

        trace!("decode", "start");

        self.decode_symbols(&mut input)?;
        self.decode_instructions(&mut input)?;

        trace!("decode", "end");

        // Implicit top-level frame
        let return_info = self.allocate(NULL.into(), NULL.into())?.into();
        self.stack = self.allocate(return_info, NULL.set_tag(FRAME_TAG).into())?;

        Ok(())
    }

    fn decode_symbols(&mut self, input: &mut impl Iterator<Item = u8>) -> Result<(), Error> {
        for _ in 0..Self::decode_mere_integer(input).ok_or(Error::MissingInteger)? {
            let symbol = self.create_symbol(NULL, 0)?;
            self.push(symbol.into())?;
        }

        let mut length = 0;
        let mut name = NULL;
        let mut byte = input.next().ok_or(Error::EndOfInput)?;

        if byte != b';' {
            loop {
                match byte {
                    character @ (b',' | b';') => {
                        let symbol = self.create_symbol(name, length)?;
                        self.push(symbol.into())?;

                        length = 0;
                        name = NULL;

                        if character == b';' {
                            break;
                        }
                    }
                    character => {
                        length += 1;
                        name = self.append(Number::new(character as i64).into(), name)?;
                    }
                }

                byte = input.next().ok_or(Error::EndOfInput)?;
            }
        }

        let rib = self.allocate(
            Number::new(Primitive::Rib as i64).into(),
            NULL.set_tag(Type::Procedure as u8).into(),
        )?;

        self.initialize_symbol(rib.into())?;
        self.initialize_symbol(NULL.into())?;
        self.initialize_symbol(TRUE.into())?;
        self.initialize_symbol(FALSE.into())?;

        self.symbols = self.stack;
        self.stack = NULL;

        Ok(())
    }

    fn create_symbol(&mut self, name: Cons, length: i64) -> Result<Cons, Error> {
        let string = self.allocate(
            Number::new(length).into(),
            name.set_tag(Type::String as u8).into(),
        )?;

        self.allocate(FALSE.into(), string.set_tag(Type::Symbol as u8).into())
    }

    fn initialize_symbol(&mut self, value: Value) -> Result<(), Error> {
        let symbol = self.allocate(value, NULL.set_tag(Type::Symbol as u8).into())?;

        self.push(symbol.into())
    }

    fn decode_instructions(&mut self, input: &mut impl Iterator<Item = u8>) -> Result<(), Error> {
        while let Some((instruction, integer)) = self.decode_instruction(input)? {
            trace!("instruction", instruction);

            let (car, tag) = match instruction {
                code::Instruction::RETURN_CALL => {
                    self.push(self.program_counter.into())?;
                    self.program_counter = NULL;

                    (self.decode_operand(integer)?, Instruction::CALL)
                }
                code::Instruction::CALL => (self.decode_operand(integer)?, Instruction::CALL),
                code::Instruction::CLOSURE => {
                    let code = self.allocate(
                        Number::new(integer as i64).into(),
                        self.program_counter.into(),
                    )?;
                    self.program_counter = self.pop()?.try_into()?;

                    (
                        self.allocate(code.into(), NULL.set_tag(Type::Procedure as u8).into())?
                            .into(),
                        Instruction::CONSTANT,
                    )
                }
                code::Instruction::SET => (self.decode_operand(integer)?, Instruction::SET),
                code::Instruction::GET => (self.decode_operand(integer)?, Instruction::GET),
                code::Instruction::CONSTANT => {
                    (self.decode_operand(integer)?, Instruction::CONSTANT)
                }
                code::Instruction::IF => {
                    let then = self.program_counter;
                    self.program_counter = self.pop()?.try_into()?;

                    (then.into(), Instruction::IF)
                }
                _ => return Err(Error::IllegalInstruction),
            };

            self.program_counter = self.append(car, self.program_counter.set_tag(tag))?;
        }

        self.stack = NULL;

        Ok(())
    }

    fn decode_instruction(
        &mut self,
        input: &mut impl Iterator<Item = u8>,
    ) -> Result<Option<(u8, u64)>, Error> {
        let Some(byte) = input.next() else {
            return Ok(None);
        };

        Ok(Some((
            byte & code::INSTRUCTION_MASK,
            Self::decode_integer(input, byte >> code::INSTRUCTION_BITS)
                .ok_or(Error::MissingOperand)?,
        )))
    }

    fn decode_operand(&self, integer: u64) -> Result<Value, Error> {
        let index = Number::new((integer >> 1) as i64);
        let is_symbol = integer & 1 == 0;

        trace!("operand", index);
        trace!("symbol", is_symbol);

        Ok(if is_symbol {
            self.car(self.tail(self.symbols, index)?)
        } else {
            index.into()
        })
    }

    fn decode_mere_integer(input: &mut impl Iterator<Item = u8>) -> Option<u64> {
        let byte = input.next()?;
        Self::decode_integer_rest(input, byte, code::INTEGER_BASE)
    }

    fn decode_integer(input: &mut impl Iterator<Item = u8>, rest: u8) -> Option<u64> {
        Self::decode_integer_rest(input, rest, code::SHORT_INTEGER_BASE)
    }

    fn decode_integer_rest(
        input: &mut impl Iterator<Item = u8>,
        rest: u8,
        base: u64,
    ) -> Option<u64> {
        let mut x = rest;
        let mut y = 0;

        while x & 1 != 0 {
            y *= code::INTEGER_BASE;
            x = input.next()?;
            y += (x >> 1) as u64;
        }

        Some(y * base + (rest >> 1) as u64)
    }
}

impl<T: Device, const N: usize> Display for Vm<N, T> {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        writeln!(formatter, "program counter: {}", self.program_counter)?;
        writeln!(formatter, "stack: {}", self.stack)?;
        writeln!(formatter, "symbols: {}", self.symbols)?;

        for index in 0..self.allocation_index / 2 {
            let index = self.allocation_start() + 2 * index;
            let cons = Cons::new(index as u64);

            write!(
                formatter,
                "{:02x}: {} {}",
                index,
                self.car(cons),
                self.cdr(cons)
            )?;

            if index == self.program_counter.index() {
                write!(formatter, " <- program counter")?;
            } else if index == self.stack.index() {
                write!(formatter, " <- stack")?;
            } else if index == self.symbols.index() {
                write!(formatter, " <- symbols")?;
            } else if index == self.cons.index() {
                write!(formatter, " <- cells")?;
            }

            writeln!(formatter)?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{symbol_index, FixedBufferDevice};
    use alloc::vec;
    use code::{encode, Instruction, Operand, Program};
    use std::format;

    const HEAP_SIZE: usize = 1 << 9;

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
    fn create_with_too_small_heap() {
        assert_eq!(
            Vm::<0, FixedBufferDevice<0, 0>>::new(Default::default()).unwrap_err(),
            Error::OutOfMemory
        );
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

        vm.collect_garbages(None).unwrap();
        vm.run().unwrap();

        insta::assert_display_snapshot!(vm);
    }

    #[test]
    fn create_list() {
        let mut vm = create_vm();

        let list = vm.append(Number::new(1).into(), NULL).unwrap();

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
            vm.collect_garbages(None).unwrap();

            insta::assert_display_snapshot!(vm);
        }

        #[test]
        fn collect_stack() {
            let mut vm = create_vm();

            vm.push(Number::new(42).into()).unwrap();
            vm.collect_garbages(None).unwrap();

            insta::assert_display_snapshot!(vm);
        }

        #[test]
        fn collect_deep_stack() {
            let mut vm = create_vm();

            vm.push(Number::new(1).into()).unwrap();
            vm.push(Number::new(2).into()).unwrap();
            vm.collect_garbages(None).unwrap();

            insta::assert_display_snapshot!(vm);
        }

        #[test]
        fn collect_cycle() {
            let mut vm = create_vm();

            let cons = vm.allocate(ZERO.into(), ZERO.into()).unwrap();
            *vm.cdr_mut(cons) = cons.into();

            vm.collect_garbages(None).unwrap();

            insta::assert_display_snapshot!(vm);
        }
    }

    mod instruction {
        use super::*;

        fn run_program(program: &Program) {
            let mut vm = create_vm();

            vm.initialize(encode(program)).unwrap();

            vm.run().unwrap()
        }

        #[test]
        fn run_nothing() {
            run_program(&Program::new(vec![], vec![]));
        }

        #[test]
        fn constant() {
            run_program(&Program::new(
                vec![],
                vec![Instruction::Constant(Operand::Integer(42))],
            ));
        }

        #[test]
        fn close() {
            run_program(&Program::new(
                vec![],
                vec![
                    Instruction::Closure(0, vec![Instruction::Call(Operand::Integer(1), true)]),
                    Instruction::Constant(Operand::Integer(0)),
                ],
            ));
        }

        #[test]
        fn set_global() {
            run_program(&Program::new(
                vec!["x".into()],
                vec![
                    Instruction::Constant(Operand::Integer(42)),
                    Instruction::Set(Operand::Symbol(symbol_index::OTHER)),
                ],
            ));
        }

        #[test]
        fn set_empty_global() {
            run_program(&Program::new(
                vec!["".into()],
                vec![
                    Instruction::Constant(Operand::Integer(42)),
                    Instruction::Set(Operand::Symbol(symbol_index::OTHER)),
                ],
            ));
        }

        #[test]
        fn set_second_empty_global() {
            run_program(&Program::new(
                vec!["".into(), "".into()],
                vec![
                    Instruction::Constant(Operand::Integer(42)),
                    Instruction::Set(Operand::Symbol(symbol_index::OTHER + 1)),
                ],
            ));
        }

        #[test]
        fn set_local() {
            run_program(&Program::new(
                vec![],
                vec![
                    Instruction::Constant(Operand::Integer(0)),
                    Instruction::Constant(Operand::Integer(42)),
                    Instruction::Set(Operand::Integer(0)),
                ],
            ));
        }

        #[test]
        fn get_global() {
            run_program(&Program::new(
                vec!["x".into()],
                vec![Instruction::Get(Operand::Symbol(symbol_index::OTHER))],
            ));
        }

        #[test]
        fn get_empty_global() {
            run_program(&Program::new(
                vec!["".into()],
                vec![Instruction::Get(Operand::Symbol(symbol_index::OTHER))],
            ));
        }

        #[test]
        fn get_second_empty_global() {
            run_program(&Program::new(
                vec!["".into(), "".into()],
                vec![Instruction::Get(Operand::Symbol(symbol_index::OTHER + 1))],
            ));
        }

        #[test]
        fn get_local() {
            run_program(&Program::new(
                vec![],
                vec![
                    Instruction::Constant(Operand::Integer(42)),
                    Instruction::Get(Operand::Integer(0)),
                ],
            ));
        }

        #[test]
        fn r#if() {
            run_program(&Program::new(
                vec![],
                vec![
                    Instruction::Constant(Operand::Integer(0)),
                    Instruction::Get(Operand::Symbol(symbol_index::NULL)),
                    Instruction::Constant(Operand::Integer(0)),
                    Instruction::Constant(Operand::Integer(3)),
                    Instruction::Get(Operand::Symbol(symbol_index::FALSE)),
                    Instruction::If(
                        vec![Instruction::Call(Operand::Symbol(symbol_index::RIB), true)],
                        vec![Instruction::Call(Operand::Symbol(symbol_index::RIB), true)],
                    ),
                ],
            ));
        }
    }
}
