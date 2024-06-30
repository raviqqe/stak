use crate::{
    cons::{Cons, Tag, NEVER},
    number::Number,
    r#type::Type,
    symbol_index,
    value::Value,
    Error, StackSlot,
};
use code::{SYMBOL_SEPARATOR, SYMBOL_TERMINATOR};
use core::{
    fmt::{self, Display, Formatter},
    mem::replace,
};
use stak_code as code;

const CONS_FIELD_COUNT: usize = 2;

macro_rules! trace {
    ($prefix:literal, $data:expr) => {
        #[cfg(feature = "trace_instruction")]
        std::eprintln!("{}: {}", $prefix, $data);
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

pub struct Memory<'a> {
    pub program_counter: Cons,
    pub stack: Cons,
    pub r#false: Cons,
    pub register: Cons,
    pub allocation_index: usize,
    pub space: bool,
    pub heap: &'a mut [Value],
}

impl<'a> Memory<'a> {
    pub fn new(heap: &'a mut [Value]) -> Result<Self, Error> {
        let mut memory = Self {
            program_counter: NEVER,
            stack: NEVER,
            r#false: NEVER,
            register: NEVER,
            allocation_index: 0,
            space: false,
            heap,
        };

        let null = memory
            .allocate_unchecked(NEVER.set_tag(Type::Null as Tag).into(), Default::default())?;
        // Do not use `NEVER` for `car` for an `equal?` procedure.
        let r#true = memory.allocate_unchecked(
            null.set_tag(Type::Boolean as Tag).into(),
            Default::default(),
        )?;
        memory.r#false =
            memory.allocate_unchecked(r#true.set_tag(Type::Boolean as Tag).into(), null.into())?;

        Ok(memory)
    }

    pub fn tail(&self, mut list: Cons, mut index: Number) -> Cons {
        while index != Number::default() {
            list = self.cdr(list).assume_cons();
            index = Number::new(index.to_i64() - 1);
        }

        list
    }

    /// Returns a cons.
    pub fn cons(&mut self, car: Value, cdr: Cons) -> Result<Cons, Error> {
        self.allocate(car.set_tag(Type::Pair as Tag), cdr.into())
    }

    /// Returns a current stack.
    pub const fn stack(&self) -> Cons {
        self.stack
    }

    /// Pushes a value to a stack.
    pub fn push(&mut self, value: Value) -> Result<(), Error> {
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

    /// Peeks a value at the top of a stack.
    pub fn top(&mut self) -> Value {
        debug_assert_ne!(self.stack, self.null());

        self.car(self.stack)
    }

    /// Allocates a cons on heap.
    pub fn allocate(&mut self, car: Value, cdr: Value) -> Result<Cons, Error> {
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

    fn allocate_unchecked(&mut self, car: Value, cdr: Value) -> Result<Cons, Error> {
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

    pub fn get(&self, index: usize) -> Value {
        assert_heap_access!(self, index);
        self.heap[index]
    }

    pub fn get_mut(&mut self, index: usize) -> &mut Value {
        assert_heap_access!(self, index);
        &mut self.heap[index]
    }

    /// Returns a value of a `car` field in a cons.
    pub fn car(&self, cons: Cons) -> Value {
        self.get(cons.index())
    }

    /// Returns a value of a `cdr` field in a cons.
    pub fn cdr(&self, cons: Cons) -> Value {
        self.get(cons.index() + 1)
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
        *self.get_mut(cons.index() + index) = value;
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
            value.set_tag(self.get(cons.index() + index).tag()),
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

    // Garbage collection

    fn collect_garbages(&mut self, cons: Option<&mut Cons>) -> Result<(), Error> {
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
            *self.get_mut(index) = self.copy_value(self.get(index))?;
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
    pub fn initialize(&mut self, input: impl IntoIterator<Item = u8>) -> Result<(), Error> {
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
            .allocate(Number::new(0).into(), self.null().into())?
            .into();
        let continuation = self.allocate(codes, self.null().into())?.into();
        self.stack = self.cons(continuation, self.null().set_tag(StackSlot::Frame as _))?;

        self.register = NEVER;

        Ok(())
    }

    fn decode_symbols(&mut self, input: &mut impl Iterator<Item = u8>) -> Result<Cons, Error> {
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

    fn build_symbol_table(&mut self, symbols: Cons) -> Result<(), Error> {
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

    fn initialize_symbol(&mut self, name: Option<Cons>, value: Value) -> Result<(), Error> {
        let symbol = self.allocate(
            name.unwrap_or(self.register)
                .set_tag(Type::Symbol as Tag)
                .into(),
            value,
        )?;

        self.push(symbol.into())
    }

    fn create_string(&mut self, name: Cons, length: i64) -> Result<Cons, Error> {
        self.allocate(
            name.set_tag(Type::String as Tag).into(),
            Number::new(length).into(),
        )
    }

    fn decode_instructions(&mut self, input: &mut impl Iterator<Item = u8>) -> Result<(), Error> {
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
}

impl<'a> Display for Memory<'a> {
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

    const HEAP_SIZE: usize = 1 << 9;

    fn create_heap() -> [Value; HEAP_SIZE] {
        [Default::default(); HEAP_SIZE]
    }

    macro_rules! assert_snapshot {
        ($memory:expr) => {
            #[cfg(not(feature = "gc_always"))]
            insta::assert_snapshot!($memory);

            let _ = $memory;
        };
    }

    #[test]
    fn create() {
        let mut heap = create_heap();
        let vm = Memory::new(&mut heap).unwrap();

        assert_snapshot!(vm);
    }

    #[test]
    fn create_list() {
        let mut heap = create_heap();
        let mut vm = Memory::new(&mut heap).unwrap();

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
        let vm = Memory::new(&mut heap).unwrap();

        assert_eq!(
            Value::from(vm.boolean(false)).to_cons().unwrap(),
            vm.boolean(false)
        );
    }

    #[test]
    fn convert_true() {
        let mut heap = create_heap();
        let vm = Memory::new(&mut heap).unwrap();

        assert_eq!(
            Value::from(vm.boolean(true)).to_cons().unwrap(),
            vm.boolean(true)
        );
    }

    #[test]
    fn convert_null() {
        let mut heap = create_heap();
        let vm = Memory::new(&mut heap).unwrap();

        assert_eq!(Value::from(vm.null()).to_cons().unwrap(), vm.null());
    }

    mod stack {
        use super::*;

        #[test]
        fn push_and_pop() {
            let mut heap = create_heap();
            let mut vm = Memory::new(&mut heap).unwrap();

            vm.stack = vm.null();
            vm.push(Number::new(42).into()).unwrap();

            assert_eq!(vm.pop(), Number::new(42).into());
        }

        #[test]
        fn push_and_pop_twice() {
            let mut heap = create_heap();
            let mut vm = Memory::new(&mut heap).unwrap();

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
            let mut vm = Memory::new(&mut heap).unwrap();

            vm.allocate(Number::default().into(), Number::default().into())
                .unwrap();
            vm.collect_garbages(None).unwrap();

            assert_snapshot!(vm);
        }

        #[test]
        fn collect_stack() {
            let mut heap = create_heap();
            let mut vm = Memory::new(&mut heap).unwrap();

            vm.stack = vm.null();
            vm.push(Number::new(42).into()).unwrap();
            vm.collect_garbages(None).unwrap();

            assert_snapshot!(vm);
        }

        #[test]
        fn collect_deep_stack() {
            let mut heap = create_heap();
            let mut vm = Memory::new(&mut heap).unwrap();

            vm.stack = vm.null();
            vm.push(Number::new(1).into()).unwrap();
            vm.push(Number::new(2).into()).unwrap();
            vm.collect_garbages(None).unwrap();

            assert_snapshot!(vm);
        }

        #[test]
        fn collect_cycle() {
            let mut heap = create_heap();
            let mut vm = Memory::new(&mut heap).unwrap();

            let cons = vm
                .allocate(Number::default().into(), Number::default().into())
                .unwrap();
            vm.set_cdr(cons, cons.into());

            vm.collect_garbages(None).unwrap();

            assert_snapshot!(vm);
        }
    }
}
