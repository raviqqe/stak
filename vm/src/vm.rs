use crate::{
    cons::Cons, instruction::Instruction, number::Number, primitive::Primitive, r#type::Type,
    value::Value, Error,
};
use core::{
    fmt::{self, Display, Formatter},
    mem::replace,
    ops::{Add, Div, Mul, Sub},
};
use device::Device;

const CONS_FIELD_COUNT: usize = 2;
const ZERO: Number = Number::new(0);
// TODO Should we use Cons::new(0)?
const DUMMY_CONS: Cons = Cons::dummy(0);
const SINGLETON_CDR: Cons = DUMMY_CONS.set_tag(Type::Singleton as u8);
// TODO Should we use Cons::new(0).set_tag(u8::MAX)?
const MOVED_CAR: Cons = Cons::dummy(1);
const FRAME_TAG: u8 = 1;

macro_rules! assert_index_range {
    ($self:expr, $cons:expr) => {
        debug_assert!(
            $cons == DUMMY_CONS
                || $self.allocation_start() <= $cons.index()
                    && $cons.index() < $self.allocation_end()
        );
    };
}

macro_rules! assert_cell_index {
    ($index:expr) => {
        debug_assert!($index < 2);
    };
}

#[derive(Debug)]
pub struct Vm<const N: usize, T: Device> {
    device: T,
    program_counter: Cons,
    stack: Cons,
    symbols: Cons,
    // TODO Remove this? Does it degrade performance significantly?
    r#false: Cons,
    allocation_index: usize,
    space: bool,
    heap: [Value; N],
}

impl<const N: usize, T: Device> Vm<N, T> {
    const SPACE_SIZE: usize = N / 2;

    pub fn new(device: T) -> Result<Self, Error> {
        let mut vm = Self {
            device,
            program_counter: DUMMY_CONS,
            stack: DUMMY_CONS,
            symbols: DUMMY_CONS,
            r#false: DUMMY_CONS,
            allocation_index: 0,
            space: false,
            heap: [ZERO.into(); N],
        };

        let r#true = vm.allocate_raw(ZERO.into(), SINGLETON_CDR.into())?;
        let null = vm.allocate_raw(ZERO.into(), SINGLETON_CDR.into())?;
        vm.r#false = vm.allocate_raw(r#true.into(), null.set_tag(Type::Singleton as u8).into())?;

        vm.stack = null;
        vm.program_counter = null;

        Ok(vm)
    }

    pub fn run(&mut self) -> Result<(), Error> {
        while self.program_counter != self.null()? {
            let instruction = Cons::try_from(self.cdr(self.program_counter))?;

            #[cfg(feature = "trace")]
            std::eprintln!("instruction: {}", instruction);

            match instruction.tag() {
                Instruction::CALL => {
                    let r#return = instruction == self.null()?;
                    let mut procedure = self.procedure()?;

                    if Cons::try_from(self.cdr(procedure))?.tag() != Type::Procedure as u8 {
                        return Err(Error::ProcedureExpected);
                    }

                    match self.code(procedure) {
                        Value::Cons(code) => {
                            let argument_count = Number::try_from(self.car(self.stack))?;
                            let parameter_count = self.car(code).try_into()?;

                            // TODO Support variadic arguments.
                            if argument_count != parameter_count {
                                return Err(Error::ArgumentCount);
                            }

                            let mut last_argument = self.tail(self.stack, parameter_count)?;

                            if r#return {
                                *self.cdr_mut(last_argument) = self.frame()?.into();
                                // Drop an argument count.
                                self.pop()?;
                            } else {
                                *self.cell_mut(0)? = last_argument.into();
                                *self.cell_mut(1)? = procedure.into();

                                // Reuse an argument count cons as a new frame.
                                *self.car_mut(self.stack) = self
                                    .allocate(
                                        self.cdr(self.program_counter),
                                        self.cdr(last_argument),
                                    )?
                                    .into();

                                last_argument = self.take_cell(0)?.try_into()?;
                                procedure = self.take_cell(1)?.try_into()?;

                                *self.cdr_mut(last_argument) = self.stack.into();
                            }

                            // Set an environment.
                            *self.cdr_value_mut(self.cdr(last_argument))? =
                                Cons::try_from(self.cdr(procedure))?
                                    .set_tag(FRAME_TAG)
                                    .into();
                            self.program_counter =
                                self.cdr(self.code(procedure).try_into()?).try_into()?;
                        }
                        Value::Number(primitive) => {
                            // Drop an argument count.
                            self.pop()?;
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
                    self.program_counter = (if self.pop()? == self.r#false.into() {
                        self.cdr(self.program_counter)
                    } else {
                        self.car(self.program_counter)
                    })
                    .try_into()?;
                }
                _ => return Err(Error::IllegalInstruction),
            }

            #[cfg(feature = "trace")]
            std::eprintln!("vm:\n{}", self);
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
        if self.stack == self.null()? {
            return Err(Error::StackUnderflow);
        }

        let value = self.car(self.stack);
        self.stack = self.cdr(self.stack).try_into()?;
        Ok(value)
    }

    fn allocate(&mut self, car: Value, cdr: Value) -> Result<Cons, Error> {
        let cons = self.allocate_raw(car, cdr)?;

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
        }

        self.take_allocation_cell()?.try_into()
    }

    fn allocate_raw(&mut self, car: Value, cdr: Value) -> Result<Cons, Error> {
        if self.is_out_of_memory() {
            return Err(Error::OutOfMemory);
        }

        let cons = Cons::new((self.allocation_start() + self.allocation_index) as u64);

        assert_index_range!(self, cons);

        *self.car_mut(cons) = car;
        *self.cdr_mut(cons) = cdr;

        self.allocation_index += CONS_FIELD_COUNT;

        debug_assert!(self.allocation_index <= Self::SPACE_SIZE);

        Ok(cons)
    }

    fn is_out_of_memory(&self) -> bool {
        self.allocation_index >= Self::SPACE_SIZE
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
            self.r#true()
        } else {
            self.r#false.into()
        }
    }

    fn r#true(&self) -> Value {
        self.car(self.r#false)
    }

    fn null(&self) -> Result<Cons, Error> {
        Ok(Cons::try_from(self.cdr(self.r#false))?.set_tag(Type::Pair as u8))
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
                let cons = self.allocate(car, self.stack.set_tag(Type::Procedure as u8).into())?;

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
                    .map_err(|_| Error::WriteOutput)?;
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

    // GC escape cells

    fn take_cell(&mut self, index: usize) -> Result<Value, Error> {
        assert_cell_index!(index);

        Ok(replace(
            self.cell_mut(index)?,
            match index {
                0 => ZERO.into(),
                1 => SINGLETON_CDR.into(),
                _ => return Err(Error::CellIndexOutOfRange),
            },
        ))
    }

    fn cell_mut(&mut self, index: usize) -> Result<&mut Value, Error> {
        assert_cell_index!(index);

        (match index {
            0 => Self::car_value_mut,
            1 => Self::cdr_value_mut,
            _ => return Err(Error::CellIndexOutOfRange),
        })(self, self.r#true())
    }

    fn take_allocation_cell(&mut self) -> Result<Value, Error> {
        Ok(replace(self.allocation_cell_mut()?, SINGLETON_CDR.into()))
    }

    fn allocation_cell_mut(&mut self) -> Result<&mut Value, Error> {
        Ok(self.cdr_mut(self.null()?))
    }

    // Garbage collection

    fn collect_garbages(&mut self) -> Result<(), Error> {
        self.allocation_index = 0;
        self.space = !self.space;

        self.program_counter = self.copy_cons(self.program_counter)?;
        self.stack = self.copy_cons(self.stack)?;
        self.symbols = self.copy_cons(self.symbols)?;
        self.r#false = self.copy_cons(self.r#false)?;

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
        Ok(if cons == DUMMY_CONS {
            cons
        } else if self.car(cons) == MOVED_CAR.into() {
            // Get a forward pointer.
            self.cdr(cons).try_into()?
        } else {
            let copy = self.allocate_raw(self.car(cons), self.cdr(cons))?;

            *self.car_mut(cons) = MOVED_CAR.into();
            // Set a forward pointer.
            *self.cdr_mut(cons) = copy.into();

            copy
        }
        .set_tag(cons.tag()))
    }

    // Input decoding

    pub fn initialize(&mut self, input: impl IntoIterator<Item = u8>) -> Result<(), Error> {
        let mut input = input.into_iter();

        self.program_counter = self.null()?;
        self.stack = self.null()?;

        self.decode_symbols(&mut input)?;
        self.decode_instructions(&mut input)?;

        // Implicit top-level frame
        let return_info = self
            .allocate(self.null()?.into(), self.null()?.into())?
            .into();
        self.stack = self.allocate(return_info, self.null()?.set_tag(FRAME_TAG).into())?;

        Ok(())
    }

    fn decode_symbols(&mut self, input: &mut impl Iterator<Item = u8>) -> Result<(), Error> {
        let mut length = 0;
        let mut name = self.null()?;

        loop {
            match input.next().ok_or(Error::EndOfInput)? {
                character @ (b',' | b';') => {
                    let string = self.allocate(
                        Number::new(length).into(),
                        name.set_tag(Type::String as u8).into(),
                    )?;
                    let symbol = self.allocate(
                        self.r#false.into(),
                        string.set_tag(Type::Symbol as u8).into(),
                    )?;
                    self.push(symbol.into())?;

                    length = 0;
                    name = self.null()?;

                    if character == b';' {
                        break;
                    }
                }
                character => {
                    length += 1;
                    name = self.append(Number::new(character as u64).into(), name)?;
                }
            }
        }

        let rib = self.allocate(
            Number::new(Primitive::Rib as u64).into(),
            DUMMY_CONS.set_tag(Type::Procedure as u8).into(),
        )?;

        self.initialize_symbol(rib.into())?;
        self.initialize_symbol(self.null()?.into())?;
        self.initialize_symbol(self.r#true())?;
        self.initialize_symbol(self.r#false.into())?;

        self.symbols = self.stack;
        self.stack = self.null()?;

        Ok(())
    }

    fn initialize_symbol(&mut self, value: Value) -> Result<(), Error> {
        let symbol = self.allocate(value, DUMMY_CONS.set_tag(Type::Symbol as u8).into())?;

        self.push(symbol.into())
    }

    fn decode_instructions(&mut self, input: &mut impl Iterator<Item = u8>) -> Result<(), Error> {
        while let Some(instruction) = input.next() {
            #[cfg(feature = "trace")]
            std::eprintln!("decoded-instruction: {}", instruction);

            let (car, tag) = match instruction {
                code::Instruction::RETURN_CALL => {
                    self.push(self.program_counter.into())?;
                    self.program_counter = self.null()?;

                    (self.decode_operand(input)?, Instruction::CALL)
                }
                code::Instruction::CALL => (self.decode_operand(input)?, Instruction::CALL),
                code::Instruction::CLOSE => {
                    let instructions = self.pop()?;
                    let code = self.allocate(
                        Number::new(Self::decode_integer(input).ok_or(Error::MissingOperand)?)
                            .into(),
                        instructions,
                    )?;

                    (
                        self.allocate(
                            code.into(),
                            self.null()?.set_tag(Type::Procedure as u8).into(),
                        )?
                        .into(),
                        Instruction::CONSTANT,
                    )
                }
                code::Instruction::SET => (self.decode_operand(input)?, Instruction::SET),
                code::Instruction::GET => (self.decode_operand(input)?, Instruction::GET),
                code::Instruction::CONSTANT => (self.decode_operand(input)?, Instruction::CONSTANT),
                code::Instruction::IF => {
                    let then = self.program_counter;
                    self.program_counter = self.pop()?.try_into()?;

                    (then.into(), Instruction::IF)
                }
                _ => return Err(Error::IllegalInstruction),
            };

            self.program_counter = self.append(car, self.program_counter.set_tag(tag))?;
        }

        self.stack = self.null()?;

        Ok(())
    }

    fn decode_operand(&self, input: &mut impl Iterator<Item = u8>) -> Result<Value, Error> {
        let integer = Self::decode_integer(input).ok_or(Error::MissingOperand)?;
        let index = Number::new(integer >> 1);

        Ok(if integer & 1 == 0 {
            self.car(self.tail(self.symbols, Number::new(index.to_u64()))?)
        } else {
            index.into()
        })
    }

    fn decode_integer(input: &mut impl Iterator<Item = u8>) -> Option<u64> {
        let mut y = 0;

        while {
            y *= code::INTEGER_BASE;
            let x = input.next()? as i8;

            y += (if x < 0 { -1 } else { 1 } * x) as u64;

            x < 0
        } {}

        Some(y)
    }
}

impl<T: Device, const N: usize> Display for Vm<N, T> {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        for index in 0..self.allocation_index / 2 {
            let cons = Cons::new((self.allocation_start() + 2 * index) as u64);

            writeln!(
                formatter,
                "{:02x}: {} {}",
                2 * index,
                self.car(cons),
                self.cdr(cons)
            )?;
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

        vm.collect_garbages().unwrap();
        vm.run().unwrap();

        insta::assert_display_snapshot!(vm);
    }

    #[test]
    fn create_list() {
        let mut vm = create_vm();

        let list = vm
            .append(Number::new(1).into(), vm.null().unwrap())
            .unwrap();

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
                    Instruction::Close(0),
                    Instruction::Constant(Operand::Integer(0)),
                    Instruction::Call(Operand::Integer(1), true),
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
