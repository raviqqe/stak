use crate::{
    cons::{Cons, NEVER},
    number::Number,
    primitive::Primitive,
    r#type::Type,
    value::{TypedValue, Value},
    Error,
};
use core::{
    fmt::{self, Display, Formatter},
    mem::replace,
    ops::{Add, Div, Mul, Sub},
};
use device::Device;

const CONS_FIELD_COUNT: usize = 2;
const ZERO: Number = Number::new(0);
const FRAME_TAG: u8 = 1;

macro_rules! trace {
    ($prefix:literal, $data:expr) => {
        #[cfg(feature = "trace")]
        std::eprintln!("{}: {}", $prefix, $data);
    };
}

macro_rules! trace_heap {
    ($self:expr) => {
        #[cfg(feature = "trace_heap")]
        std::eprintln!("{}", $self);
    };
}

macro_rules! debug_assert {
    ($condition:expr) => {
        #[cfg(feature = "debug")]
        assert!($condition);
    };
}

macro_rules! debug_assert_eq {
    ($lhs:expr, $rhs:expr) => {
        #[cfg(feature = "debug")]
        assert_eq!($lhs, $rhs);
    };
}

macro_rules! assert_heap_access {
    ($self:expr, $index:expr) => {
        assert_heap_cons!(
            $self,
            Cons::new(($index / CONS_FIELD_COUNT * CONS_FIELD_COUNT) as u64)
        );
    };
}

macro_rules! assert_heap_cons {
    ($self:expr, $cons:expr) => {
        debug_assert!(
            $cons == NEVER
                || $self.allocation_start() <= $cons.index()
                    && $cons.index() < $self.allocation_end()
        );
    };
}

macro_rules! assert_heap_value {
    ($self:expr, $cons:expr) => {
        #[cfg(feature = "debug")]
        if let Some(cons) = $cons.to_cons() {
            assert_heap_cons!($self, cons);
        }
    };
}

struct ArgumentInfo {
    // A count does not include a variadic argument.
    count: Number,
    variadic: bool,
}

#[derive(Debug)]
pub struct Vm<'a, T: Device> {
    device: T,
    program_counter: Cons,
    stack: Cons,
    r#false: Cons,
    temporary: Cons,
    allocation_index: usize,
    space: bool,
    heap: &'a mut [Value],
}

// Note that some routines look unnecessarily complicated as we need to mark all
// volatile variables live across garbage collections.
impl<'a, T: Device> Vm<'a, T> {
    pub fn new(heap: &'a mut [Value], device: T) -> Result<Self, Error> {
        let mut vm = Self {
            device,
            program_counter: NEVER,
            stack: NEVER,
            r#false: NEVER,
            temporary: NEVER,
            allocation_index: 0,
            space: false,
            heap,
        };

        let null =
            vm.allocate_unchecked(Default::default(), NEVER.set_tag(Type::Null as u8).into())?;
        let r#true = vm.allocate_unchecked(
            Default::default(),
            NEVER.set_tag(Type::Boolean as u8).into(),
        )?;
        vm.r#false =
            vm.allocate_unchecked(null.into(), r#true.set_tag(Type::Boolean as u8).into())?;

        Ok(vm)
    }

    #[cfg_attr(feature = "no_inline", inline(never))]
    pub fn run(&mut self) -> Result<(), Error> {
        while self.program_counter != self.null() {
            let instruction = self.cdr(self.program_counter).assume_cons();

            trace!("instruction", instruction.tag());

            match instruction.tag() {
                code::Instruction::CALL => self.call(instruction)?,
                code::Instruction::SET => self.set()?,
                code::Instruction::GET => self.get()?,
                code::Instruction::CONSTANT => self.constant()?,
                code::Instruction::IF => self.r#if()?,
                code::Instruction::NOP => self.advance_program_counter(),
                _ => return Err(Error::IllegalInstruction),
            }

            trace_heap!(self);
        }

        Ok(())
    }

    #[cfg_attr(feature = "no_inline", inline(never))]
    fn call(&mut self, instruction: Cons) -> Result<(), Error> {
        let r#return = instruction == self.null();
        let procedure = self.procedure();

        trace!("procedure", procedure);
        trace!("return", r#return);

        if self.environment(procedure).tag() != Type::Procedure as u8 {
            return Err(Error::ProcedureExpected);
        }

        match self.code(procedure).to_typed() {
            TypedValue::Cons(code) => {
                let arguments = Self::parse_argument_count(self.argument_count());
                let parameters = Self::parse_argument_count(self.car(code).assume_number());

                trace!("argument count", arguments.count);
                trace!("argument variadic", arguments.variadic);
                trace!("parameter count", parameters.count);
                trace!("parameter variadic", parameters.variadic);

                self.temporary = procedure;

                let mut list = if arguments.variadic {
                    self.pop()?.assume_cons()
                } else {
                    self.null()
                };

                for _ in 0..arguments.count.to_i64() {
                    let value = self.pop()?;
                    list = self.cons(value, list)?;
                }

                // Use a `program_counter` field as an escape cell for a procedure.
                let program_counter = self.program_counter;
                self.program_counter = self.temporary;
                self.temporary = list;

                let continuation = if r#return {
                    self.continuation()
                } else {
                    self.allocate(self.cdr(program_counter), self.stack.into())?
                };
                self.stack = self.allocate(
                    continuation.into(),
                    self.environment(self.program_counter)
                        .set_tag(FRAME_TAG)
                        .into(),
                )?;
                self.program_counter = self
                    .cdr(self.code(self.program_counter).assume_cons())
                    .assume_cons();

                for _ in 0..parameters.count.to_i64() {
                    if self.temporary == self.null() {
                        return Err(Error::ArgumentCount);
                    }

                    self.push(self.car(self.temporary))?;
                    self.temporary = self.cdr(self.temporary).assume_cons();
                }

                if parameters.variadic {
                    self.push(self.temporary.into())?;
                } else if self.temporary != self.null() {
                    return Err(Error::ArgumentCount);
                }
            }
            TypedValue::Number(primitive) => {
                self.operate_primitive(primitive.to_i64() as u8)?;
                self.advance_program_counter();
            }
        }

        Ok(())
    }

    #[cfg_attr(feature = "no_inline", inline(never))]
    fn set(&mut self) -> Result<(), Error> {
        let operand = self.operand_variable();
        let value = self.pop()?;

        self.set_car(operand, value);
        self.advance_program_counter();

        Ok(())
    }

    #[cfg_attr(feature = "no_inline", inline(never))]
    fn get(&mut self) -> Result<(), Error> {
        let operand = self.operand_variable();

        trace!("operand", operand);

        let value = self.car(operand);

        trace!("value", value);

        self.push(value)?;
        self.advance_program_counter();

        Ok(())
    }

    #[cfg_attr(feature = "no_inline", inline(never))]
    fn constant(&mut self) -> Result<(), Error> {
        let constant = self.operand();

        trace!("constant", constant);

        self.push(constant)?;
        self.advance_program_counter();

        Ok(())
    }

    #[cfg_attr(feature = "no_inline", inline(never))]
    fn r#if(&mut self) -> Result<(), Error> {
        self.program_counter = (if self.pop()? == self.boolean(false).into() {
            self.cdr(self.program_counter)
        } else {
            self.operand()
        })
        .assume_cons();

        Ok(())
    }

    fn parse_argument_count(info: Number) -> ArgumentInfo {
        let info = info.to_i64();

        ArgumentInfo {
            count: Number::new(info / 2),
            variadic: info & 1 == 1,
        }
    }

    #[cfg_attr(feature = "no_inline", inline(never))]
    fn advance_program_counter(&mut self) {
        self.program_counter = self.cdr(self.program_counter).assume_cons();

        if self.program_counter == self.null() {
            let continuation = self.continuation();

            self.program_counter = self.car(continuation).assume_cons();
            // Keep a value at the top of a stack.
            self.set_cdr(self.stack, self.cdr(continuation));
        }
    }

    fn operand(&self) -> Value {
        self.car(self.program_counter)
    }

    fn operand_variable(&self) -> Cons {
        self.resolve_variable(self.operand())
    }

    fn resolve_variable(&self, operand: Value) -> Cons {
        match operand.to_typed() {
            TypedValue::Cons(cons) => cons, // Direct reference to a symbol
            TypedValue::Number(index) => self.tail(self.stack, index),
        }
    }

    // (code . environment)
    fn procedure(&self) -> Cons {
        self.car(self.resolve_variable(self.cdr_value(self.operand())))
            .assume_cons()
    }

    fn argument_count(&self) -> Number {
        self.car_value(self.operand()).assume_number()
    }

    // (parameter-count . instruction-list) | primitive
    fn code(&self, procedure: Cons) -> Value {
        self.car(procedure)
    }

    fn environment(&self, procedure: Cons) -> Cons {
        self.cdr(procedure).assume_cons()
    }

    // (program-counter . stack)
    fn continuation(&self) -> Cons {
        let mut stack = self.stack;

        while self.cdr(stack).assume_cons().tag() != FRAME_TAG {
            stack = self.cdr(stack).assume_cons();
        }

        self.car(stack).assume_cons()
    }

    fn tail(&self, mut list: Cons, mut index: Number) -> Cons {
        while index != ZERO {
            list = self.cdr(list).assume_cons();
            index = Number::new(index.to_i64() - 1);
        }

        list
    }

    fn cons(&mut self, car: Value, cdr: Cons) -> Result<Cons, Error> {
        self.allocate(car, cdr.into())
    }

    fn push(&mut self, value: Value) -> Result<(), Error> {
        self.stack = self.cons(value, self.stack)?;

        Ok(())
    }

    fn pop(&mut self) -> Result<Value, Error> {
        if self.stack == self.null() {
            return Err(Error::StackUnderflow);
        }

        let value = self.car(self.stack);
        self.stack = self.cdr(self.stack).assume_cons();
        Ok(value)
    }

    fn top(&self) -> Value {
        self.car(self.stack)
    }

    fn set_top(&mut self, value: Value) {
        self.set_car(self.stack, value);
    }

    #[cfg_attr(feature = "no_inline", inline(never))]
    fn allocate(&mut self, car: Value, cdr: Value) -> Result<Cons, Error> {
        let mut cons = self.allocate_unchecked(car, cdr)?;

        debug_assert_eq!(cons.tag(), Type::default() as u8);
        assert_heap_cons!(self, cons);
        assert_heap_value!(self, car);
        assert_heap_value!(self, cdr);

        if self.is_out_of_memory() || cfg!(feature = "gc_always") {
            self.collect_garbages(Some(&mut cons))?;
        }

        Ok(cons)
    }

    #[cfg_attr(feature = "no_inline", inline(never))]
    fn allocate_unchecked(&mut self, car: Value, cdr: Value) -> Result<Cons, Error> {
        if self.is_out_of_memory() {
            return Err(Error::OutOfMemory);
        }

        let cons = Cons::new(self.allocation_end() as u64);
        self.allocation_index += CONS_FIELD_COUNT;

        assert_heap_cons!(self, cons);

        self.set_car(cons, car);
        self.set_cdr(cons, cdr);

        debug_assert!(self.allocation_index <= self.space_size());

        Ok(cons)
    }

    fn is_out_of_memory(&self) -> bool {
        self.allocation_index >= self.space_size()
    }

    fn space_size(&self) -> usize {
        self.heap.len() / 2
    }

    fn allocation_start(&self) -> usize {
        if self.space {
            self.space_size()
        } else {
            0
        }
    }

    fn allocation_end(&self) -> usize {
        self.allocation_start() + self.allocation_index
    }

    fn heap(&self, index: usize) -> Value {
        assert_heap_access!(self, index);
        self.heap[index]
    }

    fn heap_mut(&mut self, index: usize) -> &mut Value {
        assert_heap_access!(self, index);
        &mut self.heap[index]
    }

    fn car(&self, cons: Cons) -> Value {
        self.heap(cons.index())
    }

    fn cdr(&self, cons: Cons) -> Value {
        self.heap(cons.index() + 1)
    }

    fn unchecked_car(&self, cons: Cons) -> Value {
        self.heap[cons.index()]
    }

    fn unchecked_cdr(&self, cons: Cons) -> Value {
        self.heap[cons.index() + 1]
    }

    fn car_value(&self, cons: Value) -> Value {
        self.car(cons.assume_cons())
    }

    fn cdr_value(&self, cons: Value) -> Value {
        self.cdr(cons.assume_cons())
    }

    fn set_car(&mut self, cons: Cons, value: Value) {
        *self.heap_mut(cons.index()) = value
    }

    fn set_cdr(&mut self, cons: Cons, value: Value) {
        *self.heap_mut(cons.index() + 1) = value;
    }

    fn set_unchecked_car(&mut self, cons: Cons, value: Value) {
        self.heap[cons.index()] = value
    }

    fn set_unchecked_cdr(&mut self, cons: Cons, value: Value) {
        self.heap[cons.index() + 1] = value;
    }

    fn set_car_value(&mut self, cons: Value, value: Value) {
        self.set_car(cons.assume_cons(), value);
    }

    fn set_cdr_value(&mut self, cons: Value, value: Value) {
        self.set_cdr(cons.assume_cons(), value);
    }

    fn boolean(&self, value: bool) -> Cons {
        if value {
            self.cdr(self.r#false).assume_cons()
        } else {
            self.r#false
        }
    }

    fn null(&self) -> Cons {
        self.car(self.r#false).assume_cons()
    }

    // Primitive operations

    #[cfg_attr(feature = "no_inline", inline(never))]
    fn operate_primitive(&mut self, primitive: u8) -> Result<(), Error> {
        trace!("primitive", primitive);

        match primitive {
            Primitive::RIB => {
                let [car, cdr, tag] = self.pop_arguments::<3>()?;
                let rib = self.allocate(
                    car,
                    cdr.assume_cons()
                        .set_tag(tag.assume_number().to_i64() as u8)
                        .into(),
                )?;
                self.set_top(rib.into());
            }
            Primitive::CONS => {
                let [car, cdr] = self.pop_arguments::<2>()?;
                let cons = self.allocate(
                    car,
                    cdr.to_cons()
                        .map(|cons| cons.set_tag(Type::Pair as u8).into())
                        .unwrap_or(cdr),
                )?;
                self.set_top(cons.into());
            }
            Primitive::CLOSE => {
                let cons = self.allocate(
                    self.car_value(self.top()),
                    self.cdr(self.stack)
                        .assume_cons()
                        .set_tag(Type::Procedure as u8)
                        .into(),
                )?;

                self.set_top(cons.into());
            }
            Primitive::IS_CONS => {
                self.set_top(self.boolean(self.top().is_cons()).into());
            }
            Primitive::CAR => {
                self.set_top(self.car_value(self.top()));
            }
            Primitive::CDR => {
                self.set_top(self.cdr_value(self.top()));
            }
            Primitive::TAG => {
                self.set_top(
                    Number::new(
                        self.cdr_value(self.top())
                            .to_cons()
                            .map(|cons| cons.tag() as i64)
                            .unwrap_or(Type::Pair as _),
                    )
                    .into(),
                );
            }
            Primitive::SET_CAR => {
                let [x, y] = self.pop_arguments::<2>()?;
                self.set_car_value(x, y);
                self.set_top(y);
            }
            Primitive::SET_CDR => {
                let [x, y] = self.pop_arguments::<2>()?;
                // Preserve a tag.
                self.set_cdr_value(
                    x,
                    y.to_cons()
                        .map(|cons| {
                            cons.set_tag(self.cdr(x.assume_cons()).assume_cons().tag())
                                .into()
                        })
                        .unwrap_or(y),
                );
                self.set_top(y);
            }
            Primitive::SET_TAG => {
                let [x, y] = self.pop_arguments::<2>()?;
                self.set_cdr_value(
                    x,
                    self.cdr_value(x)
                        .assume_cons()
                        .set_tag(y.assume_number().to_i64() as u8)
                        .into(),
                );
                self.set_top(y);
            }
            Primitive::EQUAL => {
                let [x, y] = self.pop_arguments::<2>()?;
                self.set_top(self.boolean(x == y).into());
            }
            Primitive::LESS_THAN => self.operate_comparison(|x, y| x < y)?,
            Primitive::ADD => self.operate_binary(Add::add)?,
            Primitive::SUBTRACT => self.operate_binary(Sub::sub)?,
            Primitive::MULTIPLY => self.operate_binary(Mul::mul)?,
            Primitive::DIVIDE => self.operate_binary(Div::div)?,
            Primitive::READ => {
                let byte = self.device.read().map_err(|_| Error::ReadInput)?;

                self.push(if let Some(byte) = byte {
                    Number::new(byte as i64).into()
                } else {
                    self.boolean(false).into()
                })?;
            }
            Primitive::WRITE => self
                .device
                .write(self.top().assume_number().to_i64() as u8)
                .map_err(|_| Error::WriteOutput)?,
            Primitive::WRITE_ERROR => self
                .device
                .write_error(self.top().assume_number().to_i64() as u8)
                .map_err(|_| Error::WriteError)?,
            Primitive::HALT => return Err(Error::Halt),
            _ => return Err(Error::IllegalPrimitive),
        }

        Ok(())
    }

    fn operate_binary(&mut self, operate: fn(i64, i64) -> i64) -> Result<(), Error> {
        let [x, y] = self.pop_number_arguments::<2>()?;

        self.set_top(Number::new(operate(x.to_i64(), y.to_i64())).into());

        Ok(())
    }

    fn operate_comparison(&mut self, operate: fn(i64, i64) -> bool) -> Result<(), Error> {
        let [x, y] = self.pop_number_arguments::<2>()?;

        self.set_top(self.boolean(operate(x.to_i64(), y.to_i64())).into());

        Ok(())
    }

    fn pop_number_arguments<const M: usize>(&mut self) -> Result<[Number; M], Error> {
        let mut numbers = [ZERO; M];

        for (index, value) in self.pop_arguments::<M>()?.into_iter().enumerate() {
            numbers[index] = value.assume_number();
        }

        Ok(numbers)
    }

    fn pop_arguments<const M: usize>(&mut self) -> Result<[Value; M], Error> {
        let mut values = [ZERO.into(); M];

        for index in 0..M - 1 {
            values[M - 1 - index] = self.pop()?;
        }

        values[0] = self.top();

        Ok(values)
    }

    // Garbage collection

    #[cfg_attr(feature = "no_inline", inline(never))]
    fn collect_garbages(&mut self, cons: Option<&mut Cons>) -> Result<(), Error> {
        self.allocation_index = 0;
        self.space = !self.space;

        self.program_counter = self.copy_cons(self.program_counter)?;
        self.stack = self.copy_cons(self.stack)?;
        self.r#false = self.copy_cons(self.r#false)?;
        self.temporary = self.copy_cons(self.temporary)?;

        if let Some(cons) = cons {
            *cons = self.copy_cons(*cons)?;
        }

        let mut index = self.allocation_start();

        while index < self.allocation_end() {
            *self.heap_mut(index) = self.copy_value(self.heap(index))?;
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
        Ok(if cons == NEVER {
            NEVER
        } else if self.unchecked_car(cons) == NEVER.into() {
            // Get a forward pointer.
            self.unchecked_cdr(cons).assume_cons()
        } else {
            let copy =
                self.allocate_unchecked(self.unchecked_car(cons), self.unchecked_cdr(cons))?;

            self.set_unchecked_car(cons, NEVER.into());
            // Set a forward pointer.
            self.set_unchecked_cdr(cons, copy.into());

            copy
        }
        .set_tag(cons.tag()))
    }

    // Initialization

    #[cfg_attr(feature = "no_inline", inline(never))]
    pub fn initialize(&mut self, input: impl IntoIterator<Item = u8>) -> Result<(), Error> {
        let mut input = input.into_iter();

        self.program_counter = self.null();
        self.stack = self.null();

        trace!("decode", "start");

        self.decode_symbols(&mut input)?;
        self.decode_instructions(&mut input)?;

        trace!("decode", "end");

        // Initialize an implicit top-level frame.
        let continuation = self
            .allocate(self.null().into(), self.null().into())?
            .into();
        self.stack = self.allocate(continuation, self.null().set_tag(FRAME_TAG).into())?;

        self.temporary = NEVER;

        Ok(())
    }

    #[cfg_attr(feature = "no_inline", inline(never))]
    fn decode_symbols(&mut self, input: &mut impl Iterator<Item = u8>) -> Result<(), Error> {
        for _ in 0..Self::decode_integer(input).ok_or(Error::MissingInteger)? {
            let symbol = self.create_symbol(self.null(), 0)?;
            self.push(symbol.into())?;
        }

        let mut length = 0;
        let mut name = self.null();
        let mut byte = input.next().ok_or(Error::EndOfInput)?;

        if byte != b';' {
            loop {
                match byte {
                    character @ (b',' | b';') => {
                        let symbol = self.create_symbol(name, length)?;
                        self.push(symbol.into())?;

                        length = 0;
                        name = self.null();

                        if character == b';' {
                            break;
                        }
                    }
                    character => {
                        length += 1;
                        name = self.cons(Number::new(character as i64).into(), name)?;
                    }
                }

                byte = input.next().ok_or(Error::EndOfInput)?;
            }
        }

        let rib = self.allocate(
            Number::new(Primitive::Rib as i64).into(),
            self.boolean(false).into(),
        )?;

        self.initialize_symbol(rib.into())?;
        self.initialize_symbol(self.null().into())?;
        self.initialize_symbol(self.boolean(true).into())?;
        self.initialize_symbol(self.boolean(false).into())?;

        // Set a rib primitive's environment to a symbol table for access from a base
        // library.
        self.set_cdr_value(
            self.car_value(self.car(self.tail(self.stack, Number::new(3)))),
            self.stack.set_tag(Type::Procedure as u8).into(),
        );

        // Allow access to a symbol table during decoding.
        self.temporary = self.stack;
        self.stack = self.null();

        Ok(())
    }

    fn create_symbol(&mut self, name: Cons, length: i64) -> Result<Cons, Error> {
        let string = self.allocate(
            Number::new(length).into(),
            name.set_tag(Type::String as u8).into(),
        )?;

        self.allocate(
            self.boolean(false).into(),
            string.set_tag(Type::Symbol as u8).into(),
        )
    }

    fn initialize_symbol(&mut self, value: Value) -> Result<(), Error> {
        let symbol = self.allocate(
            value,
            self.boolean(false).set_tag(Type::Symbol as u8).into(),
        )?;

        self.push(symbol.into())
    }

    #[cfg_attr(feature = "no_inline", inline(never))]
    fn decode_instructions(&mut self, input: &mut impl Iterator<Item = u8>) -> Result<(), Error> {
        while let Some((instruction, r#return, integer)) = self.decode_instruction(input)? {
            trace!("instruction", instruction);
            trace!("return", r#return);

            debug_assert!(instruction != code::Instruction::IF || !r#return);

            let program_counter = match instruction {
                code::Instruction::CALL => {
                    let operand = self.allocate(
                        Number::new(integer as i64).into(),
                        self.decode_operand(
                            Self::decode_integer(input).ok_or(Error::MissingOperand)?,
                        ),
                    )?;
                    self.append_instruction(instruction, operand.into(), r#return)?
                }
                code::Instruction::SET
                | code::Instruction::GET
                | code::Instruction::CONSTANT
                | code::Instruction::NOP => {
                    self.append_instruction(instruction, self.decode_operand(integer), r#return)?
                }
                code::Instruction::IF => {
                    let then = self.program_counter;

                    self.program_counter = self.pop()?.assume_cons();

                    self.append_instruction(instruction, then.into(), false)?
                }
                code::Instruction::CLOSE => {
                    let code = self.allocate(
                        Number::new(integer as i64).into(),
                        self.program_counter.into(),
                    )?;
                    let procedure =
                        self.allocate(code.into(), NEVER.set_tag(Type::Procedure as u8).into())?;

                    self.program_counter = self.pop()?.assume_cons();

                    self.append_instruction(
                        code::Instruction::CONSTANT,
                        procedure.into(),
                        r#return,
                    )?
                }
                code::Instruction::SKIP => {
                    self.tail(self.program_counter, Number::new(integer as i64))
                }
                _ => return Err(Error::IllegalInstruction),
            };

            let program_counter = replace(&mut self.program_counter, program_counter);

            if r#return {
                self.push(program_counter.into())?;
            }
        }

        Ok(())
    }

    fn append_instruction(
        &mut self,
        instruction: u8,
        operand: Value,
        r#return: bool,
    ) -> Result<Cons, Error> {
        self.cons(
            operand,
            (if r#return {
                self.null()
            } else {
                self.program_counter
            })
            .set_tag(instruction),
        )
    }

    fn decode_instruction(
        &mut self,
        input: &mut impl Iterator<Item = u8>,
    ) -> Result<Option<(u8, bool, u64)>, Error> {
        let Some(byte) = input.next() else {
            return Ok(None);
        };

        let instruction = byte & code::INSTRUCTION_MASK;

        Ok(Some((
            instruction >> 1,
            instruction & 1 != 0,
            Self::decode_short_integer(input, byte >> code::INSTRUCTION_BITS)
                .ok_or(Error::MissingOperand)?,
        )))
    }

    fn decode_operand(&self, integer: u64) -> Value {
        let index = Number::new((integer >> 1) as i64);
        let is_symbol = integer & 1 == 0;

        trace!("operand", index);
        trace!("symbol", is_symbol);

        if is_symbol {
            self.car(self.tail(self.temporary, index))
        } else {
            index.into()
        }
    }

    fn decode_integer(input: &mut impl Iterator<Item = u8>) -> Option<u64> {
        let byte = input.next()?;
        Self::decode_integer_rest(input, byte, code::INTEGER_BASE)
    }

    fn decode_short_integer(input: &mut impl Iterator<Item = u8>, rest: u8) -> Option<u64> {
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

impl<'a, T: Device> Display for Vm<'a, T> {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        writeln!(formatter, "program counter: {}", self.program_counter)?;
        writeln!(formatter, "stack: {}", self.stack)?;

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
            } else if index == self.temporary.index() {
                write!(formatter, " <- temporary")?;
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

    const HEAP_SIZE: usize = 1 << 9;

    type FakeDevice = FixedBufferDevice<16, 16, 16>;

    fn create_heap() -> [Value; HEAP_SIZE] {
        [ZERO.into(); HEAP_SIZE]
    }

    fn create_vm(heap: &mut [Value]) -> Vm<FakeDevice> {
        Vm::<_>::new(heap, FakeDevice::new()).unwrap()
    }

    macro_rules! assert_snapshot {
        ($vm:expr) => {
            #[cfg(not(feature = "gc_always"))]
            {
                use std::format;
                insta::assert_display_snapshot!($vm);
            }

            let _ = $vm;
        };
    }

    #[test]
    fn create() {
        let mut heap = create_heap();
        let vm = create_vm(&mut heap);

        assert_snapshot!(vm);
    }

    #[test]
    fn create_list() {
        let mut heap = create_heap();
        let mut vm = create_vm(&mut heap);

        assert_eq!(vm.cdr(vm.null()).to_cons().unwrap().tag(), Type::Null as u8);

        let list = vm.cons(Number::new(1).into(), vm.null()).unwrap();

        assert_eq!(vm.cdr(list).to_cons().unwrap().tag(), Type::Pair as u8);
        assert_snapshot!(vm);

        let list = vm.cons(Number::new(2).into(), list).unwrap();

        assert_eq!(vm.cdr(list).to_cons().unwrap().tag(), Type::Pair as u8);
        assert_snapshot!(vm);

        let list = vm.cons(Number::new(3).into(), list).unwrap();

        assert_eq!(vm.cdr(list).to_cons().unwrap().tag(), Type::Pair as u8);
        assert_snapshot!(vm);
    }

    #[test]
    fn convert_false() {
        let mut heap = create_heap();
        let vm = create_vm(&mut heap);

        assert_eq!(
            Value::from(vm.boolean(false)).to_cons().unwrap(),
            vm.boolean(false)
        );
    }

    #[test]
    fn convert_true() {
        let mut heap = create_heap();
        let vm = create_vm(&mut heap);

        assert_eq!(
            Value::from(vm.boolean(true)).to_cons().unwrap(),
            vm.boolean(true)
        );
    }

    #[test]
    fn convert_null() {
        let mut heap = create_heap();
        let vm = create_vm(&mut heap);

        assert_eq!(Value::from(vm.null()).to_cons().unwrap(), vm.null());
    }

    mod stack {
        use super::*;

        #[test]
        fn push_and_pop() {
            let mut heap = create_heap();
            let mut vm = create_vm(&mut heap);

            vm.push(Number::new(42).into()).unwrap();

            assert_eq!(vm.pop(), Ok(Number::new(42).into()));
        }

        #[test]
        fn push_and_pop_twice() {
            let mut heap = create_heap();
            let mut vm = create_vm(&mut heap);

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
            let mut heap = create_heap();
            let mut vm = create_vm(&mut heap);

            vm.allocate(ZERO.into(), ZERO.into()).unwrap();
            vm.collect_garbages(None).unwrap();

            assert_snapshot!(vm);
        }

        #[test]
        fn collect_stack() {
            let mut heap = create_heap();
            let mut vm = create_vm(&mut heap);

            vm.push(Number::new(42).into()).unwrap();
            vm.collect_garbages(None).unwrap();

            assert_snapshot!(vm);
        }

        #[test]
        fn collect_deep_stack() {
            let mut heap = create_heap();
            let mut vm = create_vm(&mut heap);

            vm.push(Number::new(1).into()).unwrap();
            vm.push(Number::new(2).into()).unwrap();
            vm.collect_garbages(None).unwrap();

            assert_snapshot!(vm);
        }

        #[test]
        fn collect_cycle() {
            let mut heap = create_heap();
            let mut vm = create_vm(&mut heap);

            let cons = vm.allocate(ZERO.into(), ZERO.into()).unwrap();
            vm.set_cdr(cons, cons.into());

            vm.collect_garbages(None).unwrap();

            assert_snapshot!(vm);
        }
    }

    mod instruction {
        use super::*;

        fn run_program(program: &Program) {
            let mut heap = create_heap();
            let mut vm = create_vm(&mut heap);

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
                    Instruction::Close(0, vec![Instruction::Call(0, Operand::Integer(1))]),
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
                    Instruction::If(vec![Instruction::Call(
                        0,
                        Operand::Symbol(symbol_index::RIB),
                    )]),
                ],
            ));
        }

        #[test]
        fn if_with_continuation() {
            run_program(&Program::new(
                vec![],
                vec![
                    Instruction::Constant(Operand::Integer(0)),
                    Instruction::If(vec![Instruction::Constant(Operand::Integer(1))]),
                    Instruction::Constant(Operand::Integer(2)),
                ],
            ));
        }

        #[test]
        fn if_with_skip_instruction() {
            run_program(&Program::new(
                vec![],
                vec![
                    Instruction::Constant(Operand::Integer(0)),
                    Instruction::If(vec![
                        Instruction::Constant(Operand::Integer(1)),
                        Instruction::Skip(1),
                    ]),
                    Instruction::Constant(Operand::Integer(2)),
                    Instruction::Constant(Operand::Integer(3)),
                ],
            ));
        }

        #[test]
        fn multiple_if() {
            run_program(&Program::new(
                vec![],
                vec![
                    Instruction::Constant(Operand::Integer(0)),
                    Instruction::If(vec![Instruction::Constant(Operand::Integer(1))]),
                    Instruction::Constant(Operand::Integer(2)),
                    Instruction::If(vec![Instruction::Constant(Operand::Integer(3))]),
                    Instruction::Constant(Operand::Integer(4)),
                ],
            ));
        }
    }
}
