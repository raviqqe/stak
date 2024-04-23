use crate::{
    cons::{Cons, Tag, NEVER},
    number::Number,
    primitive_set::PrimitiveSet,
    r#type::Type,
    symbol_index,
    value::{TypedValue, Value},
    Error,
};
use code::{SYMBOL_SEPARATOR, SYMBOL_TERMINATOR};
#[cfg(feature = "profile")]
use core::cell::RefCell;
use core::{
    fmt::{self, Display, Formatter},
    mem::replace,
};
use stak_code as code;

const CONS_FIELD_COUNT: usize = 2;

/// A tag for a procedure frame.
pub const FRAME_TAG: Tag = 1;

mod instruction {
    use super::*;

    pub const CONSTANT: Tag = code::Instruction::CONSTANT as _;
    pub const GET: Tag = code::Instruction::GET as _;
    pub const SET: Tag = code::Instruction::SET as _;
    pub const IF: Tag = code::Instruction::IF as _;
    pub const NOP: Tag = code::Instruction::NOP as _;
}

macro_rules! trace {
    ($prefix:literal, $data:expr) => {
        #[cfg(feature = "trace_instruction")]
        std::eprintln!("{}: {}", $prefix, $data);
    };
}

macro_rules! trace_heap {
    ($self:expr) => {
        #[cfg(feature = "trace_heap")]
        std::eprintln!("{}", $self);
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
        if $cons != NEVER {
            debug_assert!($self.allocation_start() <= $cons.index());
            debug_assert!($cons.index() < $self.allocation_end());
        }
    };
}

macro_rules! assert_heap_value {
    ($self:expr, $cons:expr) => {
        if let Some(cons) = $cons.to_cons() {
            assert_heap_cons!($self, cons);
        }
    };
}

struct Arity {
    // A count does not include a variadic argument.
    count: Number,
    variadic: bool,
}

/// A virtual machine.
pub struct Vm<'a, T: PrimitiveSet> {
    primitive_set: T,
    program_counter: Cons,
    stack: Cons,
    r#false: Cons,
    register: Cons,
    allocation_index: usize,
    space: bool,
    heap: &'a mut [Value],
    #[cfg(feature = "profile")]
    profiler: Option<RefCell<&'a mut dyn FnMut(&Self, bool)>>,
}

// Note that some routines look unnecessarily complicated as we need to mark all
// volatile variables live across garbage collections.
impl<'a, T: PrimitiveSet> Vm<'a, T> {
    /// Creates a virtual machine.
    pub fn new(heap: &'a mut [Value], primitive_set: T) -> Result<Self, T::Error> {
        let mut vm = Self {
            primitive_set,
            program_counter: NEVER,
            stack: NEVER,
            r#false: NEVER,
            register: NEVER,
            allocation_index: 0,
            space: false,
            heap,
            #[cfg(feature = "profile")]
            profiler: None,
        };

        let null =
            vm.allocate_unchecked(NEVER.set_tag(Type::Null as Tag).into(), Default::default())?;
        // Do not use `NEVER` for `car` for an `equal?` procedure.
        let r#true = vm.allocate_unchecked(
            null.set_tag(Type::Boolean as Tag).into(),
            Default::default(),
        )?;
        vm.r#false =
            vm.allocate_unchecked(r#true.set_tag(Type::Boolean as Tag).into(), null.into())?;

        Ok(vm)
    }

    #[cfg(feature = "profile")]
    pub fn with_profiler(self, profiler: &'a mut dyn FnMut(&Self, bool)) -> Self {
        Self {
            profiler: Some(profiler.into()),
            ..self
        }
    }

    /// Runs a virtual machine.
    pub fn run(&mut self) -> Result<(), T::Error> {
        while self.program_counter != self.null() {
            let instruction = self.cdr(self.program_counter).assume_cons();

            trace!("instruction", instruction.tag());

            match instruction.tag() {
                instruction::CONSTANT => self.constant()?,
                instruction::GET => self.get()?,
                instruction::SET => self.set(),
                instruction::IF => self.r#if(),
                instruction::NOP => self.advance_program_counter(),
                code => self.call(
                    instruction,
                    code as usize - code::Instruction::CALL as usize,
                )?,
            }

            trace_heap!(self);
        }

        Ok(())
    }

    fn constant(&mut self) -> Result<(), T::Error> {
        let constant = self.operand();

        trace!("constant", constant);

        self.push(constant)?;
        self.advance_program_counter();

        Ok(())
    }

    fn get(&mut self) -> Result<(), T::Error> {
        let value = self.resolve_operand(self.operand());

        trace!("operand", value);

        self.push(value)?;
        self.advance_program_counter();

        Ok(())
    }

    fn set(&mut self) {
        match self.operand().to_typed() {
            TypedValue::Cons(cons) => {
                let value = self.pop();
                self.set_cdr(cons, value);
            }
            TypedValue::Number(index) => {
                let cons = self.tail(self.stack, index);
                let value = self.pop();
                self.set_car(cons, value)
            }
        }

        self.advance_program_counter();
    }

    fn r#if(&mut self) {
        self.program_counter = (if self.pop() == self.boolean(false).into() {
            self.cdr(self.program_counter)
        } else {
            self.operand()
        })
        .assume_cons();
    }

    fn call(&mut self, instruction: Cons, arity: usize) -> Result<(), T::Error> {
        let r#return = instruction == self.null();
        let procedure = self.procedure();

        trace!("procedure", procedure);
        trace!("return", r#return);

        if self.environment(procedure).tag() != Type::Procedure as u16 {
            return Err(Error::ProcedureExpected.into());
        }

        match self.code(procedure).to_typed() {
            TypedValue::Cons(code) => {
                self.profile(false);

                let arguments = Self::parse_arity(arity);
                let parameters =
                    Self::parse_arity(self.car(code).assume_number().to_i64() as usize);

                trace!("argument count", arguments.count);
                trace!("argument variadic", arguments.variadic);
                trace!("parameter count", parameters.count);
                trace!("parameter variadic", parameters.variadic);

                self.register = procedure;

                let mut list = if arguments.variadic {
                    self.pop().assume_cons()
                } else {
                    self.null()
                };

                for _ in 0..arguments.count.to_i64() {
                    let value = self.pop();
                    list = self.cons(value, list)?;
                }

                // Use a `program_counter` field as an escape cell for a procedure.
                let program_counter = self.program_counter;
                self.program_counter = self.register;
                self.register = list;

                let continuation = if r#return {
                    self.continuation()
                } else {
                    self.allocate(program_counter.into(), self.stack.into())?
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
                    if self.register == self.null() {
                        return Err(Error::ArgumentCount.into());
                    }

                    self.push(self.car(self.register))?;
                    self.register = self.cdr(self.register).assume_cons();
                }

                if parameters.variadic {
                    self.push(self.register.into())?;
                } else if self.register != self.null() {
                    return Err(Error::ArgumentCount.into());
                }

                if r#return {
                    self.profile(true);
                }
            }
            TypedValue::Number(primitive) => {
                if Self::parse_arity(arity).variadic {
                    self.register = self.pop().assume_cons();

                    while self.register != self.null() {
                        self.push(self.car(self.register))?;
                        self.register = self.cdr(self.register).assume_cons();
                    }
                }

                T::operate(self, primitive.to_i64() as u8)?;
                self.advance_program_counter();
            }
        }

        Ok(())
    }

    const fn parse_arity(info: usize) -> Arity {
        Arity {
            count: Number::new((info / 2) as i64),
            variadic: info % 2 == 1,
        }
    }

    fn advance_program_counter(&mut self) {
        self.program_counter = self.cdr(self.program_counter).assume_cons();

        if self.program_counter == self.null() {
            self.profile(true);

            let continuation = self.continuation();

            self.program_counter = self.cdr(self.car(continuation).assume_cons()).assume_cons();
            // Keep a value at the top of a stack.
            self.set_cdr(self.stack, self.cdr(continuation));
        }
    }

    fn operand(&self) -> Value {
        self.car(self.program_counter)
    }

    fn resolve_operand(&self, operand: Value) -> Value {
        match operand.to_typed() {
            TypedValue::Cons(cons) => self.cdr(cons),
            TypedValue::Number(index) => self.car(self.tail(self.stack, index)),
        }
    }

    // (environment . code)
    fn procedure(&self) -> Cons {
        self.resolve_operand(self.operand()).assume_cons()
    }

    fn environment(&self, procedure: Cons) -> Cons {
        self.car(procedure).assume_cons()
    }

    // (parameter-count . instruction-list) | primitive-id
    fn code(&self, procedure: Cons) -> Value {
        self.cdr(procedure)
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
        while index != Number::default() {
            list = self.cdr(list).assume_cons();
            index = Number::new(index.to_i64() - 1);
        }

        list
    }

    fn cons(&mut self, car: Value, cdr: Cons) -> Result<Cons, T::Error> {
        self.allocate(car.set_tag(Type::Pair as Tag), cdr.into())
    }

    /// Returns a current stack.
    pub const fn stack(&self) -> Cons {
        self.stack
    }

    /// Pushes a value to a stack.
    pub fn push(&mut self, value: Value) -> Result<(), T::Error> {
        self.stack = self.cons(value, self.stack)?;

        Ok(())
    }

    /// Pops a value from a stack.
    pub fn pop(&mut self) -> Value {
        debug_assert_ne!(self.stack, self.null());

        let value = self.car(self.stack);
        self.stack = self.cdr(self.stack).assume_cons();
        value
    }

    /// Allocates a cons on heap.
    pub fn allocate(&mut self, car: Value, cdr: Value) -> Result<Cons, T::Error> {
        let mut cons = self.allocate_unchecked(car, cdr)?;

        debug_assert_eq!(cons.tag(), Type::default() as Tag);
        assert_heap_cons!(self, cons);
        assert_heap_value!(self, car);
        assert_heap_value!(self, cdr);

        if self.is_out_of_memory() || cfg!(feature = "gc_always") {
            self.collect_garbages(Some(&mut cons))?;
        }

        Ok(cons)
    }

    fn allocate_unchecked(&mut self, car: Value, cdr: Value) -> Result<Cons, T::Error> {
        if self.is_out_of_memory() {
            return Err(Error::OutOfMemory.into());
        }

        let cons = Cons::new(self.allocation_end() as u64);
        self.allocation_index += CONS_FIELD_COUNT;

        assert_heap_cons!(self, cons);

        self.set_raw_car(cons, car);
        self.set_raw_cdr(cons, cdr);

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

    /// Returns a value of a `car` field in a cons.
    pub fn car(&self, cons: Cons) -> Value {
        self.heap(cons.index())
    }

    /// Returns a value of a `cdr` field in a cons.
    pub fn cdr(&self, cons: Cons) -> Value {
        self.heap(cons.index() + 1)
    }

    fn unchecked_car(&self, cons: Cons) -> Value {
        self.heap[cons.index()]
    }

    fn unchecked_cdr(&self, cons: Cons) -> Value {
        self.heap[cons.index() + 1]
    }

    /// Returns a value of a `car` field in a value assumed as a cons.
    pub fn car_value(&self, cons: Value) -> Value {
        self.car(cons.assume_cons())
    }

    /// Returns a value of a `cdr` field in a value assumed as a cons.
    pub fn cdr_value(&self, cons: Value) -> Value {
        self.cdr(cons.assume_cons())
    }

    fn set_raw_field(&mut self, cons: Cons, index: usize, value: Value) {
        *self.heap_mut(cons.index() + index) = value;
    }

    fn set_raw_car(&mut self, cons: Cons, value: Value) {
        self.set_raw_field(cons, 0, value)
    }

    fn set_raw_cdr(&mut self, cons: Cons, value: Value) {
        self.set_raw_field(cons, 1, value)
    }

    fn set_field(&mut self, cons: Cons, index: usize, value: Value) {
        self.set_raw_field(
            cons,
            index,
            value.set_tag(self.heap(cons.index() + index).tag()),
        )
    }

    /// Sets a value to a `car` field in a cons.
    pub fn set_car(&mut self, cons: Cons, value: Value) {
        self.set_field(cons, 0, value)
    }

    /// Sets a value to a `cdr` field in a cons.
    pub fn set_cdr(&mut self, cons: Cons, value: Value) {
        self.set_field(cons, 1, value)
    }

    fn set_unchecked_car(&mut self, cons: Cons, value: Value) {
        self.heap[cons.index()] = value
    }

    fn set_unchecked_cdr(&mut self, cons: Cons, value: Value) {
        self.heap[cons.index() + 1] = value;
    }

    /// Sets a value to a `car` field in a value assumed as a cons.
    pub fn set_car_value(&mut self, cons: Value, value: Value) {
        self.set_car(cons.assume_cons(), value);
    }

    /// Sets a value to a `cdr` field in a value assumed as a cons.
    pub fn set_cdr_value(&mut self, cons: Value, value: Value) {
        self.set_cdr(cons.assume_cons(), value);
    }

    /// Returns a boolean value.
    pub fn boolean(&self, value: bool) -> Cons {
        if value {
            self.car(self.r#false).assume_cons()
        } else {
            self.r#false
        }
    }

    /// Returns a null value.
    pub fn null(&self) -> Cons {
        self.cdr(self.r#false).assume_cons()
    }

    fn profile(&mut self, r#return: bool) {
        #[cfg(feature = "profile")]
        if let Some(profiler) = &self.profiler {
            profiler.borrow_mut()(&self, r#return);
        }
    }

    // Garbage collection

    fn collect_garbages(&mut self, cons: Option<&mut Cons>) -> Result<(), T::Error> {
        self.allocation_index = 0;
        self.space = !self.space;

        self.program_counter = self.copy_cons(self.program_counter)?;
        self.stack = self.copy_cons(self.stack)?;
        self.r#false = self.copy_cons(self.r#false)?;
        self.register = self.copy_cons(self.register)?;

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

    fn copy_value(&mut self, value: Value) -> Result<Value, T::Error> {
        Ok(if let Some(cons) = value.to_cons() {
            self.copy_cons(cons)?.into()
        } else {
            value
        })
    }

    fn copy_cons(&mut self, cons: Cons) -> Result<Cons, T::Error> {
        Ok(if cons == NEVER {
            NEVER
        } else if self.unchecked_cdr(cons) == NEVER.into() {
            // Get a forward pointer.
            self.unchecked_car(cons).assume_cons()
        } else {
            let copy =
                self.allocate_unchecked(self.unchecked_car(cons), self.unchecked_cdr(cons))?;

            // Set a forward pointer.
            self.set_unchecked_car(cons, copy.into());
            self.set_unchecked_cdr(cons, NEVER.into());

            copy
        }
        .set_tag(cons.tag()))
    }

    // Initialization

    /// Initializes a virtual machine with bytecodes of a program.
    pub fn initialize(&mut self, input: impl IntoIterator<Item = u8>) -> Result<(), T::Error> {
        let mut input = input.into_iter();

        self.program_counter = self.null();
        self.stack = self.null();

        trace!("decode", "start");

        // Allow access to a symbol table during instruction decoding.
        self.register = self.decode_symbols(&mut input)?;
        self.stack = self.null();
        self.decode_instructions(&mut input)?;
        self.build_symbol_table(self.register)?;

        trace!("decode", "end");

        // Initialize an implicit top-level frame.
        let codes = self
            .allocate(self.boolean(false).into(), self.null().into())?
            .into();
        let continuation = self.allocate(codes, self.null().into())?.into();
        self.stack = self.cons(continuation, self.null().set_tag(FRAME_TAG))?;

        self.register = NEVER;

        Ok(())
    }

    fn decode_symbols(&mut self, input: &mut impl Iterator<Item = u8>) -> Result<Cons, T::Error> {
        // Initialize a shared empty string.
        self.register = self.create_string(self.null(), 0)?;

        for _ in 0..Self::decode_integer(input).ok_or(Error::BytecodeIntegerMissing)? {
            self.initialize_symbol(None, self.boolean(false).into())?;
        }

        let mut length = 0;
        let mut name = self.null();

        while {
            let byte = input.next().ok_or(Error::BytecodeEnd)?;

            (length, name) = if matches!(byte, SYMBOL_SEPARATOR | SYMBOL_TERMINATOR) {
                let string = self.create_string(name, length)?;
                self.initialize_symbol(Some(string), self.boolean(false).into())?;

                (0, self.null())
            } else {
                (
                    length + 1,
                    self.cons(Number::new(byte as i64).into(), name)?,
                )
            };

            byte != SYMBOL_TERMINATOR
        } {}

        let rib = self.allocate(
            NEVER.set_tag(Type::Procedure as Tag).into(),
            Number::default().into(),
        )?;

        let mut cons = self.stack;

        for value in [self.boolean(false), self.boolean(true), self.null(), rib] {
            self.set_cdr_value(self.car(cons), value.into());
            cons = self.cdr(cons).assume_cons();
        }

        Ok(self.stack)
    }

    fn build_symbol_table(&mut self, symbols: Cons) -> Result<(), T::Error> {
        self.stack = self
            .car(self.tail(symbols, Number::new(symbol_index::RIB as i64)))
            .assume_cons();
        self.register = self.cons(self.r#false.into(), symbols)?;

        let mut current = self.register;

        while self.cdr(current) != self.null().into() {
            if self.cdr_value(self.car_value(self.car_value(self.cdr(current))))
                == Number::new(0).into()
            {
                self.set_cdr(current, self.cdr_value(self.cdr(current)));
            } else {
                current = self.cdr(current).assume_cons()
            }
        }

        // Set a rib primitive's environment to a symbol table for access from a base
        // library.
        self.set_car_value(self.cdr(self.stack), self.cdr(self.register));

        Ok(())
    }

    fn initialize_symbol(&mut self, name: Option<Cons>, value: Value) -> Result<(), T::Error> {
        let symbol = self.allocate(
            name.unwrap_or(self.register)
                .set_tag(Type::Symbol as Tag)
                .into(),
            value,
        )?;

        self.push(symbol.into())
    }

    fn create_string(&mut self, name: Cons, length: i64) -> Result<Cons, T::Error> {
        self.allocate(
            name.set_tag(Type::String as Tag).into(),
            Number::new(length).into(),
        )
    }

    fn decode_instructions(
        &mut self,
        input: &mut impl Iterator<Item = u8>,
    ) -> Result<(), T::Error> {
        while let Some((instruction, r#return, integer)) = Self::decode_instruction(input)? {
            trace!("instruction", instruction);
            trace!("return", r#return);

            debug_assert!(instruction != code::Instruction::IF || !r#return);

            let program_counter = match instruction {
                code::Instruction::CONSTANT
                | code::Instruction::GET
                | code::Instruction::SET
                | code::Instruction::NOP => self.append_instruction(
                    instruction as Tag,
                    self.decode_operand(integer),
                    r#return,
                )?,
                code::Instruction::IF => {
                    let then = self.program_counter;

                    self.program_counter = self.pop().assume_cons();

                    self.append_instruction(instruction as Tag, then.into(), false)?
                }
                code::Instruction::CALL => {
                    let operand = self.decode_operand(
                        Self::decode_integer(input).ok_or(Error::BytecodeOperandMissing)?,
                    );
                    self.append_instruction(instruction as Tag + integer as Tag, operand, r#return)?
                }
                code::Instruction::CLOSE => {
                    let code = self.allocate(
                        Number::new(integer as i64).into(),
                        self.program_counter.into(),
                    )?;
                    let procedure =
                        self.allocate(NEVER.set_tag(Type::Procedure as Tag).into(), code.into())?;

                    self.program_counter = self.pop().assume_cons();

                    self.append_instruction(
                        code::Instruction::CONSTANT as Tag,
                        procedure.into(),
                        r#return,
                    )?
                }
                code::Instruction::SKIP => {
                    self.tail(self.program_counter, Number::new(integer as i64))
                }
                _ => return Err(Error::IllegalInstruction.into()),
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
        instruction: Tag,
        operand: Value,
        r#return: bool,
    ) -> Result<Cons, T::Error> {
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
        input: &mut impl Iterator<Item = u8>,
    ) -> Result<Option<(u8, bool, u64)>, T::Error> {
        let Some(byte) = input.next() else {
            return Ok(None);
        };

        let instruction = byte & code::INSTRUCTION_MASK;

        Ok(Some((
            instruction >> 1,
            instruction & 1 != 0,
            Self::decode_short_integer(input, byte >> code::INSTRUCTION_BITS)
                .ok_or(Error::BytecodeOperandMissing)?,
        )))
    }

    fn decode_operand(&self, integer: u64) -> Value {
        let index = Number::new((integer >> 1) as i64);
        let is_symbol = integer & 1 == 0;

        trace!("operand", index);
        trace!("symbol", is_symbol);

        if is_symbol {
            self.car(self.tail(self.register, index))
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

    // For primitive sets.

    /// Returns a reference to a primitive set.
    pub const fn primitive_set(&self) -> &T {
        &self.primitive_set
    }

    /// Returns a mutable reference to a primitive set.
    pub fn primitive_set_mut(&mut self) -> &mut T {
        &mut self.primitive_set
    }
}

impl<'a, T: PrimitiveSet> Display for Vm<'a, T> {
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
            } else if index == self.register.index() {
                write!(formatter, " <- register")?;
            }

            writeln!(formatter)?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::symbol_index;
    use alloc::{string::String, vec, vec::Vec};
    use code::{encode, Instruction, Operand, Program};

    const HEAP_SIZE: usize = 1 << 9;

    fn create_heap() -> [Value; HEAP_SIZE] {
        [Default::default(); HEAP_SIZE]
    }

    struct FakePrimitiveSet;

    impl PrimitiveSet for FakePrimitiveSet {
        type Error = Error;

        fn operate(_vm: &mut Vm<Self>, _primitive: u8) -> Result<(), Error> {
            Err(Error::IllegalInstruction)
        }
    }

    fn create_vm(heap: &mut [Value]) -> Vm<FakePrimitiveSet> {
        Vm::new(heap, FakePrimitiveSet).unwrap()
    }

    macro_rules! assert_snapshot {
        ($vm:expr) => {
            #[cfg(not(feature = "gc_always"))]
            insta::assert_snapshot!($vm);

            let _ = $vm;
        };
    }

    fn default_symbols() -> Vec<String> {
        vec![Default::default(); symbol_index::OTHER as usize]
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

        assert_eq!(vm.car(vm.null()).tag(), Type::Null as Tag);

        let list = vm.cons(Number::new(1).into(), vm.null()).unwrap();

        assert_eq!(vm.cdr(list).tag(), Type::Pair as Tag);
        assert_snapshot!(vm);

        let list = vm.cons(Number::new(2).into(), list).unwrap();

        assert_eq!(vm.cdr(list).tag(), Type::Pair as Tag);
        assert_snapshot!(vm);

        let list = vm.cons(Number::new(3).into(), list).unwrap();

        assert_eq!(vm.cdr(list).tag(), Type::Pair as Tag);
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

            vm.stack = vm.null();
            vm.push(Number::new(42).into()).unwrap();

            assert_eq!(vm.pop(), Number::new(42).into());
        }

        #[test]
        fn push_and_pop_twice() {
            let mut heap = create_heap();
            let mut vm = create_vm(&mut heap);

            vm.stack = vm.null();
            vm.push(Number::new(1).into()).unwrap();
            vm.push(Number::new(2).into()).unwrap();

            assert_eq!(vm.pop(), Number::new(2).into());
            assert_eq!(vm.pop(), Number::new(1).into());
        }
    }

    mod garbage_collection {
        use super::*;

        #[test]
        fn collect_cons() {
            let mut heap = create_heap();
            let mut vm = create_vm(&mut heap);

            vm.allocate(Number::default().into(), Number::default().into())
                .unwrap();
            vm.collect_garbages(None).unwrap();

            assert_snapshot!(vm);
        }

        #[test]
        fn collect_stack() {
            let mut heap = create_heap();
            let mut vm = create_vm(&mut heap);

            vm.stack = vm.null();
            vm.push(Number::new(42).into()).unwrap();
            vm.collect_garbages(None).unwrap();

            assert_snapshot!(vm);
        }

        #[test]
        fn collect_deep_stack() {
            let mut heap = create_heap();
            let mut vm = create_vm(&mut heap);

            vm.stack = vm.null();
            vm.push(Number::new(1).into()).unwrap();
            vm.push(Number::new(2).into()).unwrap();
            vm.collect_garbages(None).unwrap();

            assert_snapshot!(vm);
        }

        #[test]
        fn collect_cycle() {
            let mut heap = create_heap();
            let mut vm = create_vm(&mut heap);

            let cons = vm
                .allocate(Number::default().into(), Number::default().into())
                .unwrap();
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
            run_program(&Program::new(default_symbols(), vec![]));
        }

        #[test]
        fn constant() {
            run_program(&Program::new(
                default_symbols(),
                vec![Instruction::Constant(Operand::Integer(42))],
            ));
        }

        #[test]
        fn create_closure() {
            run_program(&Program::new(
                default_symbols(),
                vec![Instruction::Close(
                    0,
                    vec![Instruction::Constant(Operand::Integer(0))],
                )],
            ));
        }

        #[test]
        fn get_closure() {
            run_program(&Program::new(
                default_symbols(),
                vec![
                    Instruction::Close(0, vec![Instruction::Constant(Operand::Integer(0))]),
                    Instruction::Get(Operand::Integer(0)),
                ],
            ));
        }

        #[test]
        fn set_global() {
            run_program(&Program::new(
                default_symbols().into_iter().chain(["x".into()]).collect(),
                vec![
                    Instruction::Constant(Operand::Integer(42)),
                    Instruction::Set(Operand::Symbol(symbol_index::OTHER)),
                ],
            ));
        }

        #[test]
        fn set_empty_global() {
            run_program(&Program::new(
                default_symbols().into_iter().chain(["".into()]).collect(),
                vec![
                    Instruction::Constant(Operand::Integer(42)),
                    Instruction::Set(Operand::Symbol(symbol_index::OTHER)),
                ],
            ));
        }

        #[test]
        fn set_second_global() {
            run_program(&Program::new(
                default_symbols()
                    .into_iter()
                    .chain(["x".into(), "y".into()])
                    .collect(),
                vec![
                    Instruction::Constant(Operand::Integer(42)),
                    Instruction::Set(Operand::Symbol(symbol_index::OTHER + 1)),
                ],
            ));
        }

        #[test]
        fn set_second_empty_global() {
            run_program(&Program::new(
                default_symbols()
                    .into_iter()
                    .chain(["".into(), "".into()])
                    .collect(),
                vec![
                    Instruction::Constant(Operand::Integer(42)),
                    Instruction::Set(Operand::Symbol(symbol_index::OTHER + 1)),
                ],
            ));
        }

        #[test]
        fn set_local() {
            run_program(&Program::new(
                default_symbols(),
                vec![
                    Instruction::Constant(Operand::Integer(0)),
                    Instruction::Set(Operand::Integer(0)),
                ],
            ));
        }

        #[test]
        fn set_second_local() {
            run_program(&Program::new(
                default_symbols(),
                vec![
                    Instruction::Constant(Operand::Integer(0)),
                    Instruction::Constant(Operand::Integer(0)),
                    Instruction::Set(Operand::Integer(1)),
                ],
            ));
        }

        #[test]
        fn get_global() {
            run_program(&Program::new(
                default_symbols().into_iter().chain(["x".into()]).collect(),
                vec![Instruction::Get(Operand::Symbol(symbol_index::OTHER))],
            ));
        }

        #[test]
        fn get_empty_global() {
            run_program(&Program::new(
                default_symbols().into_iter().chain(["".into()]).collect(),
                vec![Instruction::Get(Operand::Symbol(symbol_index::OTHER))],
            ));
        }

        #[test]
        fn get_second_global() {
            run_program(&Program::new(
                default_symbols()
                    .into_iter()
                    .chain(["x".into(), "y".into()])
                    .collect(),
                vec![Instruction::Get(Operand::Symbol(symbol_index::OTHER + 1))],
            ));
        }

        #[test]
        fn get_second_empty_global() {
            run_program(&Program::new(
                default_symbols()
                    .into_iter()
                    .chain(["".into(), "".into()])
                    .collect(),
                vec![Instruction::Get(Operand::Symbol(symbol_index::OTHER + 1))],
            ));
        }

        #[test]
        fn get_built_in_globals() {
            run_program(&Program::new(
                default_symbols(),
                vec![
                    Instruction::Get(Operand::Symbol(symbol_index::FALSE)),
                    Instruction::Get(Operand::Symbol(symbol_index::TRUE)),
                    Instruction::Get(Operand::Symbol(symbol_index::NULL)),
                    Instruction::Get(Operand::Symbol(symbol_index::RIB)),
                ],
            ));
        }

        #[test]
        fn get_local() {
            run_program(&Program::new(
                default_symbols(),
                vec![
                    Instruction::Constant(Operand::Integer(0)),
                    Instruction::Get(Operand::Integer(0)),
                ],
            ));
        }

        #[test]
        fn get_second_local() {
            run_program(&Program::new(
                default_symbols(),
                vec![
                    Instruction::Constant(Operand::Integer(0)),
                    Instruction::Constant(Operand::Integer(0)),
                    Instruction::Get(Operand::Integer(1)),
                ],
            ));
        }

        #[test]
        fn if_with_false() {
            run_program(&Program::new(
                default_symbols(),
                vec![
                    Instruction::Get(Operand::Symbol(symbol_index::FALSE)),
                    Instruction::If(vec![Instruction::Constant(Operand::Integer(0))]),
                ],
            ));
        }

        #[test]
        fn if_with_true() {
            run_program(&Program::new(
                default_symbols(),
                vec![
                    Instruction::Get(Operand::Symbol(symbol_index::TRUE)),
                    Instruction::If(vec![Instruction::Constant(Operand::Integer(0))]),
                ],
            ));
        }

        #[test]
        fn if_with_continuation() {
            run_program(&Program::new(
                default_symbols(),
                vec![
                    Instruction::Get(Operand::Symbol(symbol_index::TRUE)),
                    Instruction::If(vec![Instruction::Constant(Operand::Integer(0))]),
                    Instruction::Constant(Operand::Integer(0)),
                ],
            ));
        }

        #[test]
        fn if_with_skip_instruction() {
            run_program(&Program::new(
                default_symbols(),
                vec![
                    Instruction::Get(Operand::Symbol(symbol_index::TRUE)),
                    Instruction::If(vec![
                        Instruction::Constant(Operand::Integer(0)),
                        Instruction::Skip(1),
                    ]),
                    Instruction::Constant(Operand::Integer(0)),
                    Instruction::Constant(Operand::Integer(0)),
                ],
            ));
        }

        #[test]
        fn multiple_if() {
            run_program(&Program::new(
                default_symbols(),
                vec![
                    Instruction::Get(Operand::Symbol(symbol_index::TRUE)),
                    Instruction::If(vec![Instruction::Constant(Operand::Integer(0))]),
                    Instruction::Get(Operand::Symbol(symbol_index::TRUE)),
                    Instruction::If(vec![Instruction::Constant(Operand::Integer(0))]),
                    Instruction::Constant(Operand::Integer(0)),
                ],
            ));
        }
    }
}
