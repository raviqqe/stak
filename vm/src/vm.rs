#[cfg(feature = "profile")]
use crate::profiler::Profiler;
use crate::{
    Error, Exception, StackSlot,
    code::{INTEGER_BASE, NUMBER_BASE, SHARE_BASE, TAG_BASE},
    cons::{Cons, NEVER},
    instruction::Instruction,
    memory::Memory,
    number::Number,
    primitive_set::PrimitiveSet,
    r#type::Type,
    value::{TypedValue, Value},
};
#[cfg(feature = "profile")]
use core::cell::RefCell;
use core::{
    fmt::{self, Display, Formatter, Write},
    pin::pin,
    task::{Context, Poll, Waker},
};

macro_rules! trace {
    ($prefix:literal, $data:expr) => {
        #[cfg(feature = "trace_instruction")]
        std::eprintln!("{}: {}", $prefix, $data);
    };
}

macro_rules! trace_memory {
    ($self:expr) => {
        #[cfg(feature = "trace_memory")]
        std::eprintln!("{}", $self);
    };
}

macro_rules! profile_event {
    ($self:expr, $name:literal) => {
        #[cfg(feature = "profile")]
        (&$self).profile_event($name);
    };
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct Arity {
    // A count does not include a variadic argument.
    count: usize,
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
    pub fn new(heap: &'a mut [Value], primitive_set: T) -> Result<Self, Error> {
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
    pub const fn primitive_set_mut(&mut self) -> &mut T {
        &mut self.primitive_set
    }

    /// Runs bytecodes on a virtual machine.
    pub fn run(&mut self) -> Result<(), T::Error> {
        let Poll::Ready(result) =
            pin!(self.run_async()).poll(&mut Context::from_waker(Waker::noop()))
        else {
            panic!("asynchronous operation not supported")
        };

        result
    }

    /// Runs bytecodes on a virtual machine asynchronously.
    pub async fn run_async(&mut self) -> Result<(), T::Error> {
        while let Err(error) = self.run_with_continuation().await {
            if error.is_critical() {
                return Err(error);
            }

            let Some(continuation) = self.memory.cdr(self.memory.null()).to_cons() else {
                return Err(error);
            };

            if self.memory.cdr(continuation).tag() != Type::Procedure as _ {
                return Err(error);
            }

            self.memory.set_register(continuation);
            let string = self.memory.build_string("")?;
            let symbol = self.memory.allocate(
                self.memory.register().into(),
                string.set_tag(Type::Symbol as _).into(),
            )?;
            let code = self.memory.allocate(
                symbol.into(),
                self.memory
                    .code()
                    .set_tag(
                        Instruction::Call as u16
                            + Self::build_arity(Arity {
                                count: 1,
                                variadic: false,
                            }) as u16,
                    )
                    .into(),
            )?;
            self.memory.set_code(code);

            self.memory.set_register(self.memory.null());
            write!(&mut self.memory, "{error}").map_err(Error::from)?;
            let code = self.memory.allocate(
                self.memory.register().into(),
                self.memory
                    .code()
                    .set_tag(Instruction::Constant as _)
                    .into(),
            )?;
            self.memory.set_code(code);
        }

        Ok(())
    }

    async fn run_with_continuation(&mut self) -> Result<(), T::Error> {
        while self.memory.code() != self.memory.null() {
            let instruction = self.memory.cdr(self.memory.code()).assume_cons();

            trace!("instruction", instruction.tag());

            match instruction.tag() {
                Instruction::CONSTANT => self.constant()?,
                Instruction::GET => self.get()?,
                Instruction::SET => self.set(),
                Instruction::IF => self.r#if(),
                code => {
                    self.call(instruction, code as usize - Instruction::CALL as usize)
                        .await?
                }
            }

            self.advance_code();

            trace_memory!(self);
        }

        Ok(())
    }

    #[inline]
    fn constant(&mut self) -> Result<(), Error> {
        let constant = self.operand();

        trace!("constant", constant);

        self.memory.push(constant)?;

        Ok(())
    }

    #[inline]
    fn get(&mut self) -> Result<(), Error> {
        let operand = self.operand_cons();
        let value = self.memory.car(operand);

        trace!("operand", operand);
        trace!("value", value);

        self.memory.push(value)?;

        Ok(())
    }

    #[inline]
    fn set(&mut self) {
        let operand = self.operand_cons();
        let value = self.memory.pop();

        trace!("operand", operand);
        trace!("value", value);

        self.memory.set_car(operand, value);
    }

    #[inline]
    fn r#if(&mut self) {
        let cons = self.memory.stack();

        if self.memory.pop() != self.memory.boolean(false).into() {
            self.memory.set_cdr(cons, self.operand());
            self.memory.set_code(cons);
        }
    }

    #[inline(always)]
    async fn call(&mut self, instruction: Cons, arity: usize) -> Result<(), T::Error> {
        let r#return = instruction == self.memory.null();
        let procedure = self.procedure();

        trace!("procedure", procedure);
        trace!("return", r#return);

        if self.environment(procedure).tag() != Type::Procedure as _ {
            return Err(Error::ProcedureExpected.into());
        }

        match self.code(procedure).to_typed() {
            TypedValue::Cons(code) => {
                #[cfg(feature = "profile")]
                self.profile_call(self.memory.code(), r#return);

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

                for _ in 0..arguments.count {
                    let value = self.memory.pop();
                    list = self.memory.cons(value, list)?;
                }

                // Use a `code` field as an escape cell for a procedure.
                let code = self.memory.code();
                self.memory.set_code(self.memory.register());
                self.memory.set_register(list);

                let continuation = if r#return {
                    self.continuation()
                } else {
                    self.memory
                        .allocate(code.into(), self.memory.stack().into())?
                };
                let stack = self.memory.allocate(
                    continuation.into(),
                    self.environment(self.memory.code())
                        .set_tag(StackSlot::Frame as _)
                        .into(),
                )?;
                self.memory.set_stack(stack);
                self.memory
                    .set_code(self.code(self.memory.code()).assume_cons());

                for _ in 0..parameters.count {
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
                    .operate(&mut self.memory, primitive.to_i64() as _)
                    .await?;
            }
        }

        Ok(())
    }

    #[inline]
    const fn parse_arity(info: usize) -> Arity {
        Arity {
            count: info / 2,
            variadic: info % 2 == 1,
        }
    }

    #[inline]
    const fn build_arity(arity: Arity) -> usize {
        2 * arity.count + arity.variadic as usize
    }

    #[inline]
    fn advance_code(&mut self) {
        let mut code = self.memory.cdr(self.memory.code()).assume_cons();

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

        self.memory.set_code(code);
    }

    const fn operand(&self) -> Value {
        self.memory.car(self.memory.code())
    }

    const fn operand_cons(&self) -> Cons {
        match self.operand().to_typed() {
            TypedValue::Cons(cons) => cons,
            TypedValue::Number(index) => self.memory.tail(self.memory.stack(), index.to_i64() as _),
        }
    }

    // (code . environment)
    const fn procedure(&self) -> Cons {
        self.memory.car(self.operand_cons()).assume_cons()
    }

    // (parameter-count . instruction-list) | primitive-id
    const fn code(&self, procedure: Cons) -> Value {
        self.memory.car(procedure)
    }

    const fn environment(&self, procedure: Cons) -> Cons {
        self.memory.cdr(procedure).assume_cons()
    }

    // (code . stack)
    const fn continuation(&self) -> Cons {
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
        profile_event!(self, "decode_start");

        let program = self.decode_ribs(&mut input.into_iter())?;
        self.memory
            .set_false(self.memory.car(program).assume_cons());
        self.memory.set_code(self.memory.cdr(program).assume_cons());

        profile_event!(self, "decode_end");

        // Initialize an implicit top-level frame.
        let codes = self
            .memory
            .cons(Number::default().into(), self.memory.null())?
            .into();
        let continuation = self.memory.cons(codes, self.memory.null())?.into();
        let stack = self.memory.allocate(
            continuation,
            self.memory.null().set_tag(StackSlot::Frame as _).into(),
        )?;
        self.memory.set_stack(stack);
        self.memory.set_register(NEVER);

        profile_event!(self, "initialization_end");

        Ok(())
    }

    fn decode_ribs(&mut self, input: &mut impl Iterator<Item = u8>) -> Result<Cons, Error> {
        while let Some(head) = input.next() {
            if head & 1 == 0 {
                let cdr = self.memory.top();
                let cons = self
                    .memory
                    .allocate(Number::from_i64((head >> 1) as _).into(), cdr)?;
                self.memory.set_top(cons.into());
            } else if head & 0b10 == 0 {
                let head = head >> 2;

                if head == 0 {
                    let value = self.memory.top();
                    let cons = self.memory.cons(value, self.memory.code())?;
                    self.memory.set_code(cons);
                } else {
                    let integer = Self::decode_integer_tail(input, head - 1, SHARE_BASE)?;
                    let index = integer >> 1;

                    if index > 0 {
                        let cons = self.memory.tail(self.memory.code(), index as usize - 1);
                        let head = self.memory.cdr(cons).assume_cons();
                        let tail = self.memory.cdr(head);
                        self.memory.set_cdr(head, self.memory.code().into());
                        self.memory.set_cdr(cons, tail);
                        self.memory.set_code(head);
                    }

                    let value = self.memory.car(self.memory.code());

                    if integer & 1 == 0 {
                        self.memory
                            .set_code(self.memory.cdr(self.memory.code()).assume_cons());
                    }

                    self.memory.push(value)?;
                }
            } else if head & 0b100 == 0 {
                let cons = self.memory.stack();
                let cdr = self.memory.pop();
                let car = self.memory.top();
                let tag = Self::decode_integer_tail(input, head >> 3, TAG_BASE)?;
                self.memory.set_car(cons, car);
                self.memory.set_raw_cdr(cons, cdr.set_tag(tag as _));
                self.memory.set_top(cons.into());
            } else {
                self.memory.push(
                    Self::decode_number(Self::decode_integer_tail(input, head >> 3, NUMBER_BASE)?)
                        .into(),
                )?;
            }
        }

        self.memory.pop().to_cons().ok_or(Error::BytecodeEnd)
    }

    fn decode_number(integer: u128) -> Number {
        if integer & 1 == 0 {
            Number::from_i64((integer >> 1) as _)
        } else if integer & 0b10 == 0 {
            Number::from_i64(-((integer >> 2) as i64))
        } else {
            let integer = integer >> 2;
            let mantissa = if integer % 2 == 0 { 1.0 } else { -1.0 } * (integer >> 12) as f64;
            let exponent = ((integer >> 1) % (1 << 11)) as isize - 1023;

            Number::from_f64(if exponent < 0 {
                mantissa / (1u64 << exponent.abs()) as f64
            } else {
                mantissa * (1u64 << exponent) as f64
            })
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

impl<T: PrimitiveSet> Display for Vm<'_, T> {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        write!(formatter, "{}", &self.memory)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    struct FakePrimitiveSet {}

    impl PrimitiveSet for FakePrimitiveSet {
        type Error = Error;

        async fn operate(
            &mut self,
            _memory: &mut Memory<'_>,
            _primitive: usize,
        ) -> Result<(), Self::Error> {
            Ok(())
        }
    }

    type VoidVm = Vm<'static, FakePrimitiveSet>;

    #[test]
    fn arity() {
        for arity in [
            Arity {
                count: 0,
                variadic: false,
            },
            Arity {
                count: 1,
                variadic: false,
            },
            Arity {
                count: 2,
                variadic: false,
            },
            Arity {
                count: 0,
                variadic: true,
            },
            Arity {
                count: 1,
                variadic: true,
            },
            Arity {
                count: 2,
                variadic: true,
            },
        ] {
            assert_eq!(VoidVm::parse_arity(VoidVm::build_arity(arity)), arity);
        }
    }
}
