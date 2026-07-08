#[cfg(feature = "profile")]
use crate::profiler::Profiler;
use crate::{
    Error, Exception, StackSlot,
    code::{INTEGER_BASE, NUMBER_BASE, SHARE_BASE, TAG_BASE},
    cons::{Cons, NEVER},
    heap::Heap,
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
    marker::PhantomData,
};
use stak_lzss::{Lzss, MAX_WINDOW_SIZE};
use stak_util::block_on;
use winter_maybe_async::{maybe_async, maybe_await};

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
        (&$self).profile_event($name)?;
    };
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct Arity {
    // A count does not include a variadic argument.
    count: usize,
    variadic: bool,
}

/// A virtual machine.
pub struct Vm<'a, T: PrimitiveSet<H>, H = &'a mut [Value]> {
    primitive_set: T,
    memory: Memory<H>,
    #[cfg(feature = "profile")]
    profiler: Option<RefCell<&'a mut dyn Profiler<H>>>,
    _profiler: PhantomData<&'a ()>,
}

// Note that some routines look unnecessarily complicated as we need to mark all
// volatile variables live across garbage collections.
impl<'a, T: PrimitiveSet<H>, H: Heap> Vm<'a, T, H> {
    /// Creates a virtual machine.
    pub fn new(heap: H, primitive_set: T) -> Result<Self, Error> {
        Ok(Self {
            primitive_set,
            memory: Memory::new(heap)?,
            #[cfg(feature = "profile")]
            profiler: None,
            _profiler: Default::default(),
        })
    }

    /// Sets a profiler.
    #[cfg(feature = "profile")]
    pub fn with_profiler(self, profiler: &'a mut dyn Profiler<H>) -> Self {
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

    /// Runs bytecode on a virtual machine synchronously.
    ///
    /// # Panics
    ///
    /// Panics if asynchronous operations occur during the run.
    pub fn run(&mut self, input: impl IntoIterator<Item = u8>) -> Result<(), T::Error> {
        block_on!(self.run_async(input))
    }

    /// Runs bytecode on a virtual machine.
    #[cfg_attr(not(feature = "async"), doc(hidden))]
    #[maybe_async]
    pub fn run_async(&mut self, input: impl IntoIterator<Item = u8>) -> Result<(), T::Error> {
        self.initialize(input)?;

        while let Err(error) = maybe_await!(self.run_with_continuation()) {
            if error.is_critical() {
                return Err(error);
            }

            let Some(continuation) = self.memory.cdr(self.memory.null()?)?.to_cons() else {
                return Err(error);
            };

            if self.memory.cdr(continuation)?.tag() != Type::Procedure as _ {
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

            self.memory.set_register(self.memory.null()?);
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

    #[maybe_async]
    fn run_with_continuation(&mut self) -> Result<(), T::Error> {
        while self.memory.code() != self.memory.null()? {
            let instruction = self.memory.cdr(self.memory.code())?.assume_cons();

            trace!("instruction", instruction.tag());

            match instruction.tag() {
                Instruction::CONSTANT => self.constant()?,
                Instruction::GET => self.get()?,
                Instruction::SET => self.set()?,
                Instruction::IF => self.r#if()?,
                code => maybe_await!(
                    self.call(instruction, code as usize - Instruction::CALL as usize)
                )?,
            }

            self.advance_code()?;

            trace_memory!(self);
        }

        Ok(())
    }

    fn constant(&mut self) -> Result<(), Error> {
        let constant = self.operand()?;

        trace!("constant", constant);

        self.memory.push(constant)?;

        Ok(())
    }

    fn get(&mut self) -> Result<(), Error> {
        let operand = self.operand_cons()?;
        let value = self.memory.car(operand)?;

        trace!("operand", operand);
        trace!("value", value);

        self.memory.push(value)?;

        Ok(())
    }

    fn set(&mut self) -> Result<(), Error> {
        let operand = self.operand_cons()?;
        let value = self.memory.pop()?;

        trace!("operand", operand);
        trace!("value", value);

        self.memory.set_car(operand, value)?;

        Ok(())
    }

    fn r#if(&mut self) -> Result<(), Error> {
        let cons = self.memory.stack();

        if self.memory.pop()? != self.memory.boolean(false)?.into() {
            self.memory.set_cdr(cons, self.operand()?)?;
            self.memory.set_code(cons);
        }

        Ok(())
    }

    #[maybe_async]
    fn call(&mut self, instruction: Cons, arity: usize) -> Result<(), T::Error> {
        let procedure = self.procedure()?;

        trace!("procedure", procedure);

        if self.environment(procedure)?.tag() != Type::Procedure as _ {
            return Err(Error::ProcedureExpected.into());
        }

        let arguments = Self::parse_arity(arity);
        let r#return = instruction == self.memory.null()?;

        trace!("return", r#return);

        match self.code(procedure)?.to_typed() {
            TypedValue::Cons(code) => {
                #[cfg(feature = "profile")]
                self.profile_call(self.memory.code(), r#return)?;

                let parameters =
                    Self::parse_arity(self.memory.car(code)?.assume_number().to_i64() as usize);

                trace!("argument count", arguments.count);
                trace!("argument variadic", arguments.variadic);
                trace!("parameter count", parameters.count);
                trace!("parameter variadic", parameters.variadic);

                self.memory.set_register(procedure);

                let mut list = if arguments.variadic {
                    self.memory.pop()?.assume_cons()
                } else {
                    self.memory.null()?
                };

                for _ in 0..arguments.count {
                    let value = self.memory.pop()?;
                    list = self.memory.cons(value, list)?;
                }

                // Use a `code` field as an escape cell for a procedure.
                let code = self.memory.code();
                self.memory.set_code(self.memory.register());
                self.memory.set_register(list);

                let continuation = if r#return {
                    self.continuation()?
                } else {
                    self.memory
                        .allocate(code.into(), self.memory.stack().into())?
                };
                let stack = self.memory.allocate(
                    continuation.into(),
                    self.environment(self.memory.code())?
                        .set_tag(StackSlot::Frame as _)
                        .into(),
                )?;
                self.memory.set_stack(stack);
                self.memory
                    .set_code(self.code(self.memory.code())?.assume_cons());

                for _ in 0..parameters.count {
                    if self.memory.register() == self.memory.null()? {
                        return Err(Error::ArgumentCount.into());
                    }

                    self.memory.push(self.memory.car(self.memory.register())?)?;
                    self.memory
                        .set_register(self.memory.cdr(self.memory.register())?.assume_cons());
                }

                if parameters.variadic {
                    self.memory.push(self.memory.register().into())?;
                } else if self.memory.register() != self.memory.null()? {
                    return Err(Error::ArgumentCount.into());
                }
            }
            TypedValue::Number(primitive) => {
                if arguments.variadic {
                    let list = self.memory.pop()?.assume_cons();
                    self.memory.set_register(list);

                    while self.memory.register() != self.memory.null()? {
                        self.memory.push(self.memory.car(self.memory.register())?)?;
                        self.memory
                            .set_register(self.memory.cdr(self.memory.register())?.assume_cons());
                    }
                }

                maybe_await!(
                    self.primitive_set
                        .operate(&mut self.memory, primitive.to_i64() as _)
                )?;
            }
        }

        Ok(())
    }

    const fn parse_arity(info: usize) -> Arity {
        Arity {
            count: info / 2,
            variadic: info % 2 == 1,
        }
    }

    const fn build_arity(arity: Arity) -> usize {
        2 * arity.count + arity.variadic as usize
    }

    fn advance_code(&mut self) -> Result<(), Error> {
        let mut code = self.memory.cdr(self.memory.code())?.assume_cons();

        if code == self.memory.null()? {
            #[cfg(feature = "profile")]
            self.profile_return()?;

            let continuation = self.continuation()?;
            // Keep a value at the top of a stack.
            self.memory
                .set_cdr(self.memory.stack(), self.memory.cdr(continuation)?)?;

            code = self
                .memory
                .cdr(self.memory.car(continuation)?.assume_cons())?
                .assume_cons();
        }

        self.memory.set_code(code);

        Ok(())
    }

    fn operand(&self) -> Result<Value, Error> {
        self.memory.car(self.memory.code())
    }

    fn operand_cons(&self) -> Result<Cons, Error> {
        Ok(match self.operand()?.to_typed() {
            TypedValue::Cons(cons) => cons,
            TypedValue::Number(index) => {
                self.memory.tail(self.memory.stack(), index.to_i64() as _)?
            }
        })
    }

    // (code . environment)
    fn procedure(&self) -> Result<Cons, Error> {
        Ok(self.memory.car(self.operand_cons()?)?.assume_cons())
    }

    // (parameter-count . instruction-list) | primitive-id
    fn code(&self, procedure: Cons) -> Result<Value, Error> {
        self.memory.car(procedure)
    }

    fn environment(&self, procedure: Cons) -> Result<Cons, Error> {
        Ok(self.memory.cdr(procedure)?.assume_cons())
    }

    // (code . stack)
    fn continuation(&self) -> Result<Cons, Error> {
        let mut stack = self.memory.stack();

        while self.memory.cdr(stack)?.assume_cons().tag() != StackSlot::Frame as _ {
            stack = self.memory.cdr(stack)?.assume_cons();
        }

        Ok(self.memory.car(stack)?.assume_cons())
    }

    // Profiling

    #[cfg(feature = "profile")]
    fn profile_call(&self, call_code: Cons, r#return: bool) -> Result<(), Error> {
        if let Some(profiler) = &self.profiler {
            profiler
                .borrow_mut()
                .profile_call(&self.memory, call_code, r#return)?;
        }

        Ok(())
    }

    #[cfg(feature = "profile")]
    fn profile_return(&self) -> Result<(), Error> {
        if let Some(profiler) = &self.profiler {
            profiler.borrow_mut().profile_return(&self.memory)?;
        }

        Ok(())
    }

    #[cfg(feature = "profile")]
    fn profile_event(&self, name: &str) -> Result<(), Error> {
        if let Some(profiler) = &self.profiler {
            profiler.borrow_mut().profile_event(name)?;
        }

        Ok(())
    }

    // This function is public only for benchmarking.
    #[doc(hidden)]
    pub fn initialize(&mut self, input: impl IntoIterator<Item = u8>) -> Result<(), super::Error> {
        profile_event!(self, "initialization_start");
        profile_event!(self, "decode_start");

        let program = self.decode_ribs(input.into_iter())?;
        self.memory
            .set_false(self.memory.car(program)?.assume_cons())?;
        self.memory
            .set_code(self.memory.cdr(program)?.assume_cons());

        profile_event!(self, "decode_end");

        // Initialize an implicit top-level frame.
        let codes = self
            .memory
            .cons(Number::default().into(), self.memory.null()?)?
            .into();
        let continuation = self.memory.cons(codes, self.memory.null()?)?.into();
        let stack = self.memory.allocate(
            continuation,
            self.memory.null()?.set_tag(StackSlot::Frame as _).into(),
        )?;
        self.memory.set_stack(stack);
        self.memory.set_register(NEVER);

        profile_event!(self, "initialization_end");

        Ok(())
    }

    fn decode_ribs(&mut self, input: impl Iterator<Item = u8>) -> Result<Cons, Error> {
        let mut input = input.decompress::<{ MAX_WINDOW_SIZE }>();

        while let Some(head) = input.next() {
            if head & 0b1 == 0 {
                let head = head >> 1;

                if head == 0 {
                    let cons = self.memory.cons(self.memory.top()?, self.memory.code())?;
                    self.memory.set_code(cons);
                } else {
                    let integer = Self::decode_integer_tail(&mut input, head - 1, SHARE_BASE)?;
                    let index = integer >> 1;

                    if index > 0 {
                        let cons = self.memory.tail(self.memory.code(), index as usize - 1)?;
                        let head = self.memory.cdr(cons)?.assume_cons();
                        self.memory.set_cdr(cons, self.memory.cdr(head)?)?;
                        self.memory.set_cdr(head, self.memory.code().into())?;
                        self.memory.set_code(head);
                    }

                    let value = self.memory.car(self.memory.code())?;

                    if integer & 1 == 0 {
                        self.memory
                            .set_code(self.memory.cdr(self.memory.code())?.assume_cons());
                    }

                    self.memory.push(value)?;
                }
            } else if head & 0b10 == 0 {
                let head = head >> 2;
                let cdr = self.memory.pop()?;
                let car = self.memory.pop()?;

                if head == 0 {
                    let cons = self.memory.top()?.assume_cons();
                    self.memory.set_car(cons, car)?;
                    self.memory.set_cdr(cons, cdr)?;
                } else {
                    let cons =
                        self.memory.allocate(
                            car,
                            cdr.set_tag(
                                Self::decode_integer_tail(&mut input, head - 1, TAG_BASE)? as _
                            ),
                        )?;
                    self.memory.push(cons.into())?;
                }
            } else {
                self.memory
                    .push(Self::decode_number(&mut input, head)?.into())?;
            }
        }

        self.memory.pop()?.to_cons().ok_or(Error::BytecodeEnd)
    }

    fn decode_number(input: &mut impl Iterator<Item = u8>, head: u8) -> Result<Number, Error> {
        let integer = Self::decode_integer_tail(input, head >> 2, NUMBER_BASE)?;

        Ok(if integer & 1 == 0 {
            Number::from_i64((integer >> 1) as _)
        } else if integer & 0b10 == 0 {
            Number::from_i64(-((integer >> 2) as i64))
        } else {
            let integer = integer >> 2;
            let head = input.next().ok_or(Error::BytecodeEnd)?;
            let mantissa = Self::decode_integer_tail(input, head, INTEGER_BASE)?;

            Number::from_f64(
                if integer.is_multiple_of(2) { 1.0 } else { -1.0 }
                    * mantissa as f64
                    * f64::from_bits(((integer as u64 >> 1) % (1 << 11)) << 52),
            )
        })
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

impl<T: PrimitiveSet<H>, H: Heap> Display for Vm<'_, T, H> {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        write!(formatter, "{}", &self.memory)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[cfg(feature = "float")]
    use alloc::{vec, vec::Vec};

    struct FakePrimitiveSet {}

    impl<H: Heap> PrimitiveSet<H> for FakePrimitiveSet {
        type Error = Error;

        #[maybe_async]
        fn operate(
            &mut self,
            _memory: &mut Memory<H>,
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

    #[cfg(feature = "float")]
    #[test]
    fn decode_float() {
        fn encode_number(value: f64) -> Vec<u8> {
            fn encode_integer(bytes: &mut Vec<u8>, mut integer: u128, base: u128) {
                let digit = (integer % base) as u8;
                integer /= base;
                bytes.push(2 * digit + u8::from(integer != 0));

                while integer != 0 {
                    let digit = (integer % INTEGER_BASE) as u8;
                    integer /= INTEGER_BASE;
                    bytes.push(2 * digit + u8::from(integer != 0));
                }
            }

            let mut mantissa = value.abs();
            let mut exponent = 0i64;

            while mantissa != mantissa.floor() {
                mantissa *= 2.0;
                exponent -= 1;
            }

            let mut bytes = vec![];

            encode_integer(
                &mut bytes,
                3 + 4 * (u128::from(value < 0.0) + 2 * (exponent + 1023) as u128),
                NUMBER_BASE,
            );
            // Tag the leading byte as a number.
            bytes[0] = 3 + 4 * bytes[0];
            encode_integer(&mut bytes, mantissa as u128, INTEGER_BASE);

            bytes
        }

        for value in [
            0.5,
            -0.5,
            0.75,
            -0.75,
            0.125,
            1.5,
            -2.25,
            12.5,
            -40.5,
            // Small magnitudes with large negative exponents.
            0.001953125,
            -0.0009765625,
            // Mantissas beyond the former 38-bit threshold.
            137438953471.5,
            -137438953471.5,
            2251799813685247.5,
            // The minimum normal magnitude.
            f64::MIN_POSITIVE,
        ] {
            let mut input = encode_number(value).into_iter();
            let head = input.next().unwrap();

            assert_eq!(
                VoidVm::decode_number(&mut input, head).unwrap(),
                Number::from_f64(value),
            );
        }
    }

    fn literals<const N: usize>(bytes: [u8; N]) -> [u8; N] {
        bytes.map(|byte| byte * 2)
    }

    fn decode<const N: usize>(bytes: [u8; N]) -> (Cons, Memory<[Value; 1 << 9]>) {
        let mut vm = Vm::new([Default::default(); 1 << 9], FakePrimitiveSet {}).unwrap();
        let cons = vm.decode_ribs(literals(bytes).into_iter()).unwrap();

        (cons, vm.memory)
    }

    #[test]
    fn decode_shared_value() {
        // A pair whose `car` and `cdr` are the same memoized rib.
        let (rib, memory) = decode([
            // Build a placeholder pair of two zeros.
            3, 3, 5, // Memoize it into a dictionary.
            0, // Reference it twice, keeping it the first time.
            6, 2, // Build a pair of the two references.
            5,
        ]);

        let car = memory.car(rib).unwrap().assume_cons();

        assert_eq!(memory.cdr(rib).unwrap(), car.into());
        assert_eq!(memory.car(car).unwrap(), Number::from_i64(0).into());
        assert_eq!(memory.cdr(car).unwrap(), Number::from_i64(0).into());
    }

    #[test]
    fn decode_self_loop() {
        // A pair whose `cdr` points back to itself with a `car` of zero.
        let (rib, memory) = decode([
            // Build a placeholder pair of two zeros.
            3, 3, 5, // Memoize it into a dictionary.
            0, // A zero for its `car` and a reference to itself for its `cdr`.
            3, 2, // Fill the placeholder, keeping its pair tag.
            1,
        ]);

        assert_eq!(memory.car(rib).unwrap(), Number::from_i64(0).into());
        assert_eq!(memory.cdr(rib).unwrap(), rib.into());
    }

    #[test]
    fn decode_swapped_reference() {
        // Two mutually referencing pairs. Filling each pair references the other
        // one while it sits behind in the dictionary, exercising the
        // move-to-front of a non-front entry.
        let (first, memory) = decode([
            // Build and memoize the first placeholder.
            3, 3, 5, 0, // Build and memoize the second placeholder.
            3, 3, 5, 0, // Fill the second pair with a zero and the first one.
            3, 14, 1, // Bring the first pair to the front, then fill it with a
            // zero and the second one.
            6, 3, 10, 1,
        ]);

        let second = memory.cdr(first).unwrap().assume_cons();

        assert_eq!(memory.car(first).unwrap(), Number::from_i64(0).into());
        assert_eq!(memory.car(second).unwrap(), Number::from_i64(0).into());
        assert_eq!(memory.cdr(second).unwrap(), first.into());
    }
}
