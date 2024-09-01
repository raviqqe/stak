#[cfg(feature = "profile")]
use crate::profiler::Profiler;
use crate::{
    cons::{never, Cons, Tag},
    memory::Memory,
    number::Number,
    primitive_set::PrimitiveSet,
    r#type::Type,
    symbol_index,
    value::{TypedValue, Value},
    Error, StackSlot,
};
use code::{SYMBOL_SEPARATOR, SYMBOL_TERMINATOR};
#[cfg(feature = "profile")]
use core::cell::RefCell;
use core::fmt::{self, Display, Formatter};
use stak_code as code;

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

macro_rules! profile_event {
    ($self:expr, $name:literal) => {
        #[cfg(feature = "profile")]
        (&$self).profile_event($name);
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
    memory: Memory<'a>,
    #[cfg(feature = "profile")]
    profiler: Option<RefCell<&'a mut dyn Profiler>>,
}

// Note that some routines look unnecessarily complicated as we need to mark all
// volatile variables live across garbage collections.
impl<'a, T: PrimitiveSet> Vm<'a, T> {
    /// Creates a virtual machine.
    pub fn new(heap: &'a mut [Value], primitive_set: T) -> Result<Self, T::Error> {
        Ok(Self {
            primitive_set,
            memory: Memory::new(heap)?,
            #[cfg(feature = "profile")]
            profiler: None,
        })
    }

    /// Sets a profiler.
    #[cfg(feature = "profile")]
    pub fn with_profiler(self, profiler: &'a mut dyn Profiler) -> Self {
        Self {
            profiler: Some(profiler.into()),
            ..self
        }
    }

    /// Returns a reference to a primitive set.
    pub const fn primitive_set(&self) -> &T {
        &self.primitive_set
    }

    /// Returns a mutable reference to a primitive set.
    pub fn primitive_set_mut(&mut self) -> &mut T {
        &mut self.primitive_set
    }

    /// Runs a virtual machine.
    pub fn run(&mut self) -> Result<(), T::Error> {
        while self.memory.program_counter() != self.memory.null() {
            let instruction = self.memory.cdr(self.memory.program_counter()).assume_cons();

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

        self.memory.push(constant)?;
        self.advance_program_counter();

        Ok(())
    }

    fn get(&mut self) -> Result<(), T::Error> {
        let value = self.resolve_operand(self.operand());

        trace!("operand", value);

        self.memory.push(value)?;
        self.advance_program_counter();

        Ok(())
    }

    fn set(&mut self) {
        match self.operand().to_typed() {
            TypedValue::Cons(cons) => {
                let value = self.memory.pop();
                self.memory.set_cdr(cons, value);
            }
            TypedValue::Number(index) => {
                let cons = self.memory.tail(self.memory.stack(), index);
                let value = self.memory.pop();
                self.memory.set_car(cons, value)
            }
        }

        self.advance_program_counter();
    }

    fn r#if(&mut self) {
        let value = self.memory.pop();

        self.memory.set_program_counter(
            (if value == self.memory.boolean(false).into() {
                self.memory.cdr(self.memory.program_counter())
            } else {
                self.operand()
            })
            .assume_cons(),
        );
    }

    fn call(&mut self, instruction: Cons, arity: usize) -> Result<(), T::Error> {
        let r#return = instruction == self.memory.null();
        let procedure = self.procedure();

        trace!("procedure", procedure);
        trace!("return", r#return);

        if self.environment(procedure).tag() != Type::Procedure as u16 {
            return Err(Error::ProcedureExpected.into());
        }

        match self.code(procedure).to_typed() {
            TypedValue::Cons(code) => {
                #[cfg(feature = "profile")]
                self.profile_call(self.memory.program_counter(), r#return);

                let arguments = Self::parse_arity(arity);
                let parameters =
                    Self::parse_arity(self.memory.car(code).assume_number().to_i64() as usize);

                trace!("argument count", arguments.count);
                trace!("argument variadic", arguments.variadic);
                trace!("parameter count", parameters.count);
                trace!("parameter variadic", parameters.variadic);

                self.memory.set_register(procedure);

                let mut list = if arguments.variadic {
                    self.memory.pop().assume_cons()
                } else {
                    self.memory.null()
                };

                for _ in 0..arguments.count.to_i64() {
                    let value = self.memory.pop();
                    list = self.memory.cons(value, list)?;
                }

                // Use a `program_counter` field as an escape cell for a procedure.
                let program_counter = self.memory.program_counter();
                self.memory.set_program_counter(self.memory.register());
                self.memory.set_register(list);

                let continuation = if r#return {
                    self.continuation()
                } else {
                    self.memory
                        .allocate(program_counter.into(), self.memory.stack().into())?
                };
                let stack = self.memory.allocate(
                    continuation.into(),
                    self.environment(self.memory.program_counter())
                        .set_tag(StackSlot::Frame as _)
                        .into(),
                )?;
                self.memory.set_stack(stack);
                self.memory.set_program_counter(
                    self.memory
                        .cdr(self.code(self.memory.program_counter()).assume_cons())
                        .assume_cons(),
                );

                for _ in 0..parameters.count.to_i64() {
                    if self.memory.register() == self.memory.null() {
                        return Err(Error::ArgumentCount.into());
                    }

                    self.memory.push(self.memory.car(self.memory.register()))?;
                    self.memory
                        .set_register(self.memory.cdr(self.memory.register()).assume_cons());
                }

                if parameters.variadic {
                    self.memory.push(self.memory.register().into())?;
                } else if self.memory.register() != self.memory.null() {
                    return Err(Error::ArgumentCount.into());
                }
            }
            TypedValue::Number(primitive) => {
                if Self::parse_arity(arity).variadic {
                    let list = self.memory.pop().assume_cons();
                    self.memory.set_register(list);

                    while self.memory.register() != self.memory.null() {
                        self.memory.push(self.memory.car(self.memory.register()))?;
                        self.memory
                            .set_register(self.memory.cdr(self.memory.register()).assume_cons());
                    }
                }

                self.primitive_set
                    .operate(&mut self.memory, primitive.to_i64() as u8)?;
                self.advance_program_counter();
            }
        }

        Ok(())
    }

    const fn parse_arity(info: usize) -> Arity {
        Arity {
            count: Number::from_i64((info / 2) as _),
            variadic: info % 2 == 1,
        }
    }

    fn advance_program_counter(&mut self) {
        let mut code = self.memory.cdr(self.memory.program_counter()).assume_cons();

        if code == self.memory.null() {
            #[cfg(feature = "profile")]
            self.profile_return();

            let continuation = self.continuation();
            // Keep a value at the top of a stack.
            self.memory
                .set_cdr(self.memory.stack(), self.memory.cdr(continuation));

            code = self
                .memory
                .cdr(self.memory.car(continuation).assume_cons())
                .assume_cons();
        }

        self.memory.set_program_counter(code);
    }

    fn operand(&self) -> Value {
        self.memory.car(self.memory.program_counter())
    }

    fn resolve_operand(&self, operand: Value) -> Value {
        match operand.to_typed() {
            TypedValue::Cons(cons) => self.memory.cdr(cons),
            TypedValue::Number(index) => self
                .memory
                .car(self.memory.tail(self.memory.stack(), index)),
        }
    }

    // (environment . code)
    fn procedure(&self) -> Cons {
        self.resolve_operand(self.operand()).assume_cons()
    }

    fn environment(&self, procedure: Cons) -> Cons {
        self.memory.car(procedure).assume_cons()
    }

    // (parameter-count . instruction-list) | primitive-id
    fn code(&self, procedure: Cons) -> Value {
        self.memory.cdr(procedure)
    }

    // (program-counter . stack)
    fn continuation(&self) -> Cons {
        let mut stack = self.memory.stack();

        while self.memory.cdr(stack).assume_cons().tag() != StackSlot::Frame as _ {
            stack = self.memory.cdr(stack).assume_cons();
        }

        self.memory.car(stack).assume_cons()
    }

    // Profiling

    #[cfg(feature = "profile")]
    fn profile_call(&self, call_code: Cons, r#return: bool) {
        if let Some(profiler) = &self.profiler {
            profiler
                .borrow_mut()
                .profile_call(&self.memory, call_code, r#return);
        }
    }

    #[cfg(feature = "profile")]
    fn profile_return(&self) {
        if let Some(profiler) = &self.profiler {
            profiler.borrow_mut().profile_return(&self.memory);
        }
    }

    #[cfg(feature = "profile")]
    fn profile_event(&self, name: &str) {
        if let Some(profiler) = &self.profiler {
            profiler.borrow_mut().profile_event(name);
        }
    }

    /// Initializes a virtual machine with bytecodes of a program.
    pub fn initialize(&mut self, input: impl IntoIterator<Item = u8>) -> Result<(), super::Error> {
        profile_event!(self, "initialization_start");

        let mut input = input.into_iter();

        self.memory.set_program_counter(self.memory.null());
        self.memory.set_stack(self.memory.null());

        trace!("decode", "start");
        profile_event!(self, "symbol_decode_start");

        // Allow access to a symbol table during instruction decoding.
        let symbols = self.decode_symbols(&mut input)?;
        self.memory.set_register(symbols);
        self.memory.set_stack(self.memory.null());

        profile_event!(self, "symbol_decode_end");
        profile_event!(self, "instruction_decode_start");

        self.decode_instructions(&mut input)?;
        self.build_symbol_table(self.memory.register())?;

        profile_event!(self, "instruction_decode_end");

        // Initialize an implicit top-level frame.
        let codes = self
            .memory
            .allocate(Number::default().into(), self.memory.null().into())?
            .into();
        let continuation = self
            .memory
            .allocate(codes, self.memory.null().into())?
            .into();
        let stack = self.memory.cons(
            continuation,
            self.memory.null().set_tag(StackSlot::Frame as _),
        )?;
        self.memory.set_stack(stack);

        self.memory.set_register(never());

        profile_event!(self, "initialization_end");

        Ok(())
    }

    fn decode_symbols(&mut self, input: &mut impl Iterator<Item = u8>) -> Result<Cons, Error> {
        // Initialize a shared empty string.
        let string = self.create_string(self.memory.null(), 0)?;
        self.memory.set_register(string);

        for _ in 0..Self::decode_integer(input).ok_or(Error::BytecodeIntegerMissing)? {
            self.initialize_symbol(None, self.memory.boolean(false).into())?;
        }

        let mut length = 0;
        let mut name = self.memory.null();

        while {
            let byte = input.next().ok_or(Error::BytecodeEnd)?;

            (length, name) = if matches!(byte, SYMBOL_SEPARATOR | SYMBOL_TERMINATOR) {
                let string = self.create_string(name, length)?;
                self.initialize_symbol(Some(string), self.memory.boolean(false).into())?;

                (0, self.memory.null())
            } else {
                (
                    length + 1,
                    self.memory.cons(Number::from_i64(byte as _).into(), name)?,
                )
            };

            byte != SYMBOL_TERMINATOR
        } {}

        let rib = self.memory.allocate(
            never().set_tag(Type::Procedure as Tag).into(),
            Number::default().into(),
        )?;

        let mut cons = self.memory.stack();

        for value in [
            self.memory.boolean(false),
            self.memory.boolean(true),
            self.memory.null(),
            rib,
        ] {
            self.memory
                .set_cdr_value(self.memory.car(cons), value.into());
            cons = self.memory.cdr(cons).assume_cons();
        }

        Ok(self.memory.stack())
    }

    fn build_symbol_table(&mut self, symbols: Cons) -> Result<(), Error> {
        self.memory.set_stack(
            self.memory
                .car(
                    self.memory
                        .tail(symbols, Number::from_i64(symbol_index::RIB as _)),
                )
                .assume_cons(),
        );
        let symbols = self
            .memory
            .cons(self.memory.boolean(false).into(), symbols)?;
        self.memory.set_register(symbols);

        let mut current = self.memory.register();

        while self.memory.cdr(current) != self.memory.null().into() {
            if self.memory.cdr_value(
                self.memory
                    .car_value(self.memory.car_value(self.memory.cdr(current))),
            ) == Number::default().into()
            {
                self.memory
                    .set_cdr(current, self.memory.cdr_value(self.memory.cdr(current)));
            } else {
                current = self.memory.cdr(current).assume_cons()
            }
        }

        // Set a rib primitive's environment to a symbol table for access from a base
        // library.
        self.memory.set_car_value(
            self.memory.cdr(self.memory.stack()),
            self.memory.cdr(self.memory.register()),
        );

        Ok(())
    }

    fn initialize_symbol(&mut self, name: Option<Cons>, value: Value) -> Result<(), Error> {
        let symbol = self.memory.allocate(
            name.unwrap_or(self.memory.register())
                .set_tag(Type::Symbol as Tag)
                .into(),
            value,
        )?;

        self.memory.push(symbol.into())
    }

    fn create_string(&mut self, name: Cons, length: i64) -> Result<Cons, Error> {
        self.memory.allocate(
            name.set_tag(Type::String as Tag).into(),
            Number::from_i64(length).into(),
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
                    let then = self.memory.program_counter();

                    let continuation = self.memory.pop();
                    self.memory.set_program_counter(continuation.assume_cons());

                    self.append_instruction(instruction as Tag, then.into(), false)?
                }
                code::Instruction::CALL => {
                    let operand = self.decode_operand(
                        Self::decode_integer(input).ok_or(Error::BytecodeOperandMissing)?,
                    );
                    self.append_instruction(instruction as Tag + integer as Tag, operand, r#return)?
                }
                code::Instruction::CLOSE => {
                    let code = self.memory.allocate(
                        Number::from_i64(integer as _).into(),
                        self.memory.program_counter().into(),
                    )?;
                    let procedure = self
                        .memory
                        .allocate(never().set_tag(Type::Procedure as Tag).into(), code.into())?;

                    let continuation = self.memory.pop();
                    self.memory.set_program_counter(continuation.assume_cons());

                    self.append_instruction(
                        code::Instruction::CONSTANT as Tag,
                        procedure.into(),
                        r#return,
                    )?
                }
                code::Instruction::SKIP => self.memory.tail(
                    self.memory.program_counter(),
                    Number::from_i64(integer as _),
                ),
                _ => return Err(Error::IllegalInstruction),
            };

            let program_counter = {
                let counter = self.memory.program_counter();
                self.memory.set_program_counter(program_counter);
                counter
            };

            if r#return {
                self.memory.push(program_counter.into())?;
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
        self.memory.cons(
            operand,
            (if r#return {
                self.memory.null()
            } else {
                self.memory.program_counter()
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
        let index = Number::from_i64((integer >> 1) as _);
        let is_symbol = integer & 1 == 0;

        trace!("operand", index);
        trace!("symbol", is_symbol);

        if is_symbol {
            self.memory
                .car(self.memory.tail(self.memory.register(), index))
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

impl<'a, T: PrimitiveSet> Display for Vm<'a, T> {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        write!(formatter, "{}", &self.memory)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::symbol_index;
    use alloc::{string::String, vec, vec::Vec};
    use code::{encode, Instruction, Operand, Program};

    const HEAP_SIZE: usize = 1 << 9;

    struct FakePrimitiveSet;

    impl PrimitiveSet for FakePrimitiveSet {
        type Error = Error;

        fn operate(&mut self, _memory: &mut Memory, _primitive: u8) -> Result<(), Error> {
            Err(Error::IllegalInstruction)
        }
    }

    fn create_heap() -> [Value; HEAP_SIZE] {
        [Default::default(); HEAP_SIZE]
    }

    fn default_symbols() -> Vec<String> {
        vec![Default::default(); symbol_index::OTHER as usize]
    }

    fn run_program(program: &Program) {
        let mut heap = create_heap();
        let mut vm = Vm::new(&mut heap, FakePrimitiveSet).unwrap();

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
