#[cfg(feature = "profile")]
use crate::profiler::Profiler;
use crate::{
    cons::{Cons, Tag},
    memory::Memory,
    number::Number,
    primitive_set::PrimitiveSet,
    r#type::Type,
    value::{TypedValue, Value},
    Error, StackSlot,
};
use code::v2::{INTEGER_BASE, NUMBER_BASE, SHARE_BASE, TAG_BASE};
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
        let value = self.memory.car(self.operand_cons());

        trace!("operand", value);

        self.memory.push(value)?;
        self.advance_program_counter();

        Ok(())
    }

    fn set(&mut self) {
        let operand = self.operand_cons();
        let value = self.memory.pop();

        self.memory.set_car(operand, value);
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
                    .operate(&mut self.memory, primitive.to_i64() as _)?;
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

    fn operand_cons(&self) -> Cons {
        self.resolve_variable(self.operand())
    }

    fn resolve_variable(&self, operand: Value) -> Cons {
        match operand.to_typed() {
            TypedValue::Cons(cons) => cons,
            TypedValue::Number(index) => self.memory.tail(self.memory.stack(), index),
        }
    }

    // (code . environment)
    fn procedure(&self) -> Cons {
        self.memory.car(self.operand_cons()).assume_cons()
    }

    // (parameter-count . instruction-list) | primitive-id
    fn code(&self, procedure: Cons) -> Value {
        self.memory.car(procedure)
    }

    fn environment(&self, procedure: Cons) -> Cons {
        self.memory.cdr(procedure).assume_cons()
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

        let code = self.decode_ribs(&mut input.into_iter())?;
        self.memory.set_program_counter(code);

        profile_event!(self, "initialization_end");

        Ok(())
    }

    fn decode_ribs(&mut self, input: &mut impl Iterator<Item = u8>) -> Result<Cons, Error> {
        while let Some(head) = input.next() {
            if head & 1 == 0 {
                self.memory.push(
                    Self::decode_number(Self::decode_integer_tail(input, head >> 1, NUMBER_BASE)?)
                        .into(),
                )?;
            } else if head & 0b10 == 0 {
                let left = self.memory.pop();
                let right = self.memory.pop();
                let r#type = Self::decode_integer_tail(input, head >> 2, TAG_BASE)?;
                let rib = self.memory.allocate(left, right.set_tag(r#type as _))?;
                self.memory.push(rib.into())?;
            } else {
                let head = head >> 2;

                if head == 0 {
                    let value = self.memory.top();
                    self.push_to_dictionary(value)?;
                } else {
                    let integer = Self::decode_integer_tail(input, head - 1, SHARE_BASE)?;
                    let cons = self.memory.tail(
                        self.memory.program_counter(),
                        Number::from_i64((integer >> 1) as _),
                    );
                    let value = self.memory.car(cons);
                    if integer & 1 != 0 {
                        self.push_to_dictionary(value.clone())?;
                    }
                    self.memory.push(value)?;
                }
            }
        }

        self.memory.pop().to_cons().ok_or(Error::BytecodeEnd)
    }

    fn push_to_dictionary(&mut self, value: Value) -> Result<(), Error> {
        let cons = self.memory.cons(value, self.memory.program_counter())?;
        self.memory.set_program_counter(cons);

        Ok(())
    }

    fn decode_number(integer: u128) -> Number {
        let number = integer >> 1;

        if integer & 1 == 0 {
            Number::from_i64(number as _)
        } else if integer & 0b10 == 0 {
            Number::from_i64(-(number as i64))
        } else {
            panic!("floating point number not supported")
        }
    }

    fn decode_integer_tail(
        input: &mut impl Iterator<Item = u8>,
        mut x: u8,
        mut base: u128,
    ) -> Result<u128, Error> {
        let mut y = (x >> 1) as u128;

        while x & 1 != 0 {
            x = input.next().ok_or(Error::BytecodeEnd)?;
            y += (x as u128 >> 1) * base;
            base *= INTEGER_BASE;
        }

        Ok(y)
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

        fn operate(&mut self, _memory: &mut Memory, _primitive: usize) -> Result<(), Error> {
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
