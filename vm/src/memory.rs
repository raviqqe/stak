use crate::{
    Error,
    cons::{Cons, NEVER, Tag},
    number::Number,
    r#type::Type,
    value::Value,
};
use core::fmt::{self, Display, Formatter, Write};

const CONS_FIELD_COUNT: usize = 2;

macro_rules! assert_heap_index {
    ($self:expr, $index:expr, $garbage:expr) => {
        let (start, end) = if $garbage {
            let start = if $self.space { 0 } else { $self.space_size() };

            (start, start + $self.space_size())
        } else {
            ($self.allocation_start(), $self.allocation_end())
        };

        debug_assert!(start <= $index);
        debug_assert!($index < end);
    };
}

macro_rules! assert_heap_cons {
    ($self:expr, $cons:expr) => {
        if $cons != NEVER {
            assert_heap_index!($self, $cons.index(), false);
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

/// A memory on a virtual machine.
pub struct Memory<H> {
    code: Cons,
    stack: Cons,
    r#false: Cons,
    register: Cons,
    allocation_index: usize,
    space: bool,
    heap: H,
}

impl<H: Heap> Memory<H> {
    /// Creates a memory.
    pub fn new(heap: H) -> Result<Self, Error> {
        let mut memory = Self {
            code: NEVER,
            stack: NEVER,
            r#false: NEVER,
            register: NEVER,
            allocation_index: 0,
            space: false,
            heap,
        };

        // Initialize a fake false value.
        let cons = memory.allocate_unchecked(Default::default(), Default::default())?;
        memory.r#false = memory.allocate_unchecked(cons.into(), cons.into())?;

        Ok(memory)
    }

    fn heap(&self) -> &[Value] {
        self.heap.as_ref()
    }

    fn heap_mut(&mut self) -> &mut [Value] {
        self.heap.as_mut()
    }

    /// Returns a code.
    pub const fn code(&self) -> Cons {
        self.code
    }

    /// Sets a code.
    pub const fn set_code(&mut self, value: Cons) {
        self.code = value;
    }

    /// Returns a register.
    pub const fn register(&self) -> Cons {
        self.register
    }

    /// Sets a register.
    pub const fn set_register(&mut self, value: Cons) {
        self.register = value;
    }

    /// Returns a stack.
    pub const fn stack(&self) -> Cons {
        self.stack
    }

    /// Sets a stack.
    pub const fn set_stack(&mut self, value: Cons) {
        self.stack = value;
    }

    /// Returns a boolean value.
    pub fn boolean(&self, value: bool) -> Result<Cons, Error> {
        Ok(if value {
            self.cdr(self.r#false)?.assume_cons()
        } else {
            self.r#false
        })
    }

    /// Returns a null value.
    pub fn null(&self) -> Result<Cons, Error> {
        Ok(self.car(self.r#false)?.assume_cons())
    }

    /// Sets a false value.
    pub(crate) const fn set_false(&mut self, cons: Cons) {
        self.r#false = cons;
    }

    /// Pushes a value to a stack.
    pub fn push(&mut self, value: Value) -> Result<(), Error> {
        self.stack = self.cons(value, self.stack)?;

        Ok(())
    }

    /// Pops a value from a stack.
    pub fn pop(&mut self) -> Result<Value, Error> {
        debug_assert_ne!(self.stack, self.null()?);

        let value = self.car(self.stack)?;
        self.stack = self.cdr(self.stack)?.assume_cons();
        Ok(value)
    }

    /// Pops values from a stack.
    pub fn pop_many<const M: usize>(&mut self) -> Result<[Value; M], Error> {
        let mut values = [Default::default(); M];

        for index in 0..=M - 1 {
            values[M - 1 - index] = self.pop()?;
        }

        Ok(values)
    }

    /// Pops numbers from a stack.
    pub fn pop_numbers<const M: usize>(&mut self) -> Result<[Number; M], Error> {
        let mut numbers = [Default::default(); M];

        for (index, value) in self.pop_many::<M>()?.into_iter().enumerate() {
            numbers[index] = value.assume_number();
        }

        Ok(numbers)
    }

    /// Peeks a value at the top of a stack.
    pub fn top(&self) -> Result<Value, Error> {
        debug_assert_ne!(self.stack, self.null()?);

        self.car(self.stack)
    }

    /// Sets a value at the top of a stack.
    pub fn set_top(&mut self, value: Value) -> Result<(), Error> {
        self.set_car(self.stack, value)
    }

    /// Allocates a cons with a default tag of [`Type::Pair`].
    pub fn cons(&mut self, car: Value, cdr: Cons) -> Result<Cons, Error> {
        self.allocate(car, cdr.set_tag(Type::Pair as Tag).into())
    }

    /// Allocates a cons.
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
            return Err(Error::OutOfMemory);
        }

        let cons = Cons::new(self.allocation_end() as u64);
        self.allocation_index += CONS_FIELD_COUNT;

        assert_heap_cons!(self, cons);

        self.set_car(cons, car)?;
        self.set_raw_cdr(cons, cdr)?;

        debug_assert!(self.allocation_index <= self.space_size());

        Ok(cons)
    }

    fn is_out_of_memory(&self) -> bool {
        self.allocation_index >= self.space_size()
    }

    /// Returns a heap size.
    pub fn size(&self) -> usize {
        self.heap().len()
    }

    fn space_size(&self) -> usize {
        self.size() / 2
    }

    /// Returns the current allocation index relative an allocation start index.
    pub const fn allocation_index(&self) -> usize {
        self.allocation_index
    }

    /// Returns an allocation start index.
    pub fn allocation_start(&self) -> usize {
        if self.space { self.space_size() } else { 0 }
    }

    /// Returns an allocation end index.
    pub fn allocation_end(&self) -> usize {
        self.allocation_start() + self.allocation_index
    }

    fn get<const G: bool>(&self, index: usize) -> Result<Value, Error> {
        assert_heap_index!(self, index, G);

        self.heap()
            .get(index)
            .copied()
            .ok_or(Error::InvalidMemoryAccess)
    }

    fn set<const G: bool>(&mut self, index: usize, value: Value) -> Result<(), Error> {
        assert_heap_index!(self, index, G);

        *self
            .heap_mut()
            .get_mut(index)
            .ok_or(Error::InvalidMemoryAccess)? = value;

        Ok(())
    }

    /// Returns a value of a `car` field in a cons.
    pub fn car(&self, cons: Cons) -> Result<Value, Error> {
        self.get::<false>(cons.index())
    }

    /// Returns a value of a `cdr` field in a cons.
    pub fn cdr(&self, cons: Cons) -> Result<Value, Error> {
        self.get::<false>(cons.index() + 1)
    }

    fn garbage_car(&self, cons: Cons) -> Result<Value, Error> {
        self.get::<true>(cons.index())
    }

    fn garbage_cdr(&self, cons: Cons) -> Result<Value, Error> {
        self.get::<true>(cons.index() + 1)
    }

    /// Returns a value of a `car` field in a value assumed as a cons.
    pub fn car_value(&self, cons: Value) -> Result<Value, Error> {
        self.car(cons.assume_cons())
    }

    /// Returns a value of a `cdr` field in a value assumed as a cons.
    pub fn cdr_value(&self, cons: Value) -> Result<Value, Error> {
        self.cdr(cons.assume_cons())
    }

    fn set_field<const G: bool>(
        &mut self,
        cons: Cons,
        index: usize,
        value: Value,
    ) -> Result<(), Error> {
        self.set::<G>(cons.index() + index, value)
    }

    /// Sets a value to a `car` field in a cons.
    pub fn set_car(&mut self, cons: Cons, value: Value) -> Result<(), Error> {
        self.set_field::<false>(cons, 0, value)
    }

    /// Sets a value to a `cdr` field in a cons.
    pub fn set_cdr(&mut self, cons: Cons, value: Value) -> Result<(), Error> {
        // Keep an existing tag.
        self.set_field::<false>(
            cons,
            1,
            value.set_tag(self.get::<false>(cons.index() + 1)?.tag()),
        )
    }

    /// Sets a raw value to a `cdr` field in a cons overwriting its tag.
    pub fn set_raw_cdr(&mut self, cons: Cons, value: Value) -> Result<(), Error> {
        self.set_field::<false>(cons, 1, value)
    }

    fn set_garbage_car(&mut self, cons: Cons, value: Value) -> Result<(), Error> {
        self.set_field::<true>(cons, 0, value)
    }

    fn set_garbage_cdr(&mut self, cons: Cons, value: Value) -> Result<(), Error> {
        self.set_field::<true>(cons, 1, value)
    }

    /// Sets a value to a `car` field in a value assumed as a cons.
    pub fn set_car_value(&mut self, cons: Value, value: Value) -> Result<(), Error> {
        self.set_car(cons.assume_cons(), value)
    }

    /// Sets a value to a `cdr` field in a value assumed as a cons.
    pub fn set_cdr_value(&mut self, cons: Value, value: Value) -> Result<(), Error> {
        self.set_cdr(cons.assume_cons(), value)
    }

    /// Returns a tail of a list.
    pub fn tail(&self, mut list: Cons, mut index: usize) -> Result<Cons, Error> {
        while index > 0 {
            list = self.cdr(list)?.assume_cons();
            index -= 1;
        }

        Ok(list)
    }

    /// Builds a string.
    pub fn build_string(&mut self, string: &str) -> Result<Cons, Error> {
        let string = self.build_raw_string(string)?;
        let length = Number::from_i64(self.list_length(string)? as _).into();
        self.allocate(length, string.set_tag(Type::String as _).into())
    }

    /// Builds a raw string.
    pub fn build_raw_string(&mut self, string: &str) -> Result<Cons, Error> {
        let mut list = self.null()?;
        self.build_intermediate_string(string, &mut list)?;
        Ok(list)
    }

    fn build_intermediate_string(&mut self, string: &str, list: &mut Cons) -> Result<(), Error> {
        for character in string.chars().rev() {
            *list = self.cons(Number::from_i64(character as _).into(), *list)?;
        }

        Ok(())
    }

    /// Executes an operation against a value at the top of a stack.
    pub fn operate_top(
        &mut self,
        operate: impl Fn(&Self, Value) -> Result<Value, Error>,
    ) -> Result<(), Error> {
        let value = self.pop()?;
        self.push(operate(self, value)?)?;
        Ok(())
    }

    /// Calculates a length of a list.
    pub fn list_length(&self, mut list: Cons) -> Result<usize, Error> {
        let mut length = 0;

        while list != self.null()? {
            length += 1;
            list = self.cdr(list)?.assume_cons();
        }

        Ok(length)
    }

    /// Executes an unary number operation.
    pub fn operate_unary(&mut self, operate: impl Fn(Number) -> Number) -> Result<(), Error> {
        let [x] = self.pop_numbers()?;

        self.push(operate(x).into())?;

        Ok(())
    }

    /// Executes a binary number operation.
    pub fn operate_binary(&mut self, operate: fn(Number, Number) -> Number) -> Result<(), Error> {
        let [x, y] = self.pop_numbers()?;

        self.push(operate(x, y).into())?;

        Ok(())
    }

    // Garbage collection

    /// Collects garbage memory blocks.
    pub fn collect_garbages(&mut self, cons: Option<&mut Cons>) -> Result<(), Error> {
        self.allocation_index = 0;
        self.space = !self.space;

        self.code = self.copy_cons(self.code)?;
        self.stack = self.copy_cons(self.stack)?;
        self.r#false = self.copy_cons(self.r#false)?;
        self.register = self.copy_cons(self.register)?;

        if let Some(cons) = cons {
            *cons = self.copy_cons(*cons)?;
        }

        let mut index = self.allocation_start();

        while index < self.allocation_end() {
            let value = self.copy_value(self.get::<false>(index)?)?;
            self.set::<false>(index, value)?;
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
        } else if self.garbage_car(cons)? == NEVER.into() {
            // Get a forward pointer.
            self.garbage_cdr(cons)?.assume_cons()
        } else {
            let copy = self.allocate_unchecked(self.garbage_car(cons)?, self.garbage_cdr(cons)?)?;

            // Set a forward pointer.
            self.set_garbage_car(cons, NEVER.into())?;
            self.set_garbage_cdr(cons, copy.into())?;

            copy
        }
        .set_tag(cons.tag()))
    }
}

impl<H: Heap> Write for Memory<H> {
    fn write_str(&mut self, string: &str) -> fmt::Result {
        (|| -> Result<(), Error> {
            let mut list = self.null()?;
            self.build_intermediate_string(string, &mut list)?;

            if self.register() == self.null()? {
                self.set_register(list);
            } else {
                let mut head = self.register();

                while self.cdr(head)? != self.null()?.into() {
                    head = self.cdr(head)?.assume_cons();
                }

                self.set_cdr(head, list.into())?;
            }

            Ok(())
        })()
        .map_err(|_| fmt::Error)
    }
}

impl<H: Heap> Display for Memory<H> {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        writeln!(formatter, "code: {}", self.code)?;
        writeln!(formatter, "stack: {}", self.stack)?;

        for index in 0..self.allocation_index / 2 {
            let index = self.allocation_start() + 2 * index;
            let cons = Cons::new(index as u64);

            write!(
                formatter,
                "{:02x}: {} {}",
                index,
                self.car(cons).map_err(|_| fmt::Error)?,
                self.cdr(cons).map_err(|_| fmt::Error)?
            )?;

            for (cons, name) in [
                (self.code, "code"),
                (self.register, "register"),
                (self.stack, "stack"),
            ] {
                if index == cons.index() && cons != NEVER {
                    write!(formatter, " <- {name}")?;
                }
            }

            writeln!(formatter)?;
        }

        Ok(())
    }
}

/// A heap memory.
pub trait Heap: AsRef<[Value]> + AsMut<[Value]> {}

impl Heap for &mut [Value] {}

impl<const N: usize> Heap for [Value; N] {}

#[cfg(feature = "alloc")]
impl Heap for alloc::vec::Vec<Value> {}

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
        let memory = Memory::new(create_heap()).unwrap();

        assert_snapshot!(memory);
    }

    #[test]
    fn create_list() {
        let mut memory = Memory::new(create_heap()).unwrap();

        let list = memory
            .cons(Number::from_i64(1).into(), memory.null().unwrap())
            .unwrap();

        assert_eq!(memory.cdr(list).unwrap().tag(), Type::Pair as Tag);
        assert_snapshot!(memory);

        let list = memory.cons(Number::from_i64(2).into(), list).unwrap();

        assert_eq!(memory.cdr(list).unwrap().tag(), Type::Pair as Tag);
        assert_snapshot!(memory);

        let list = memory.cons(Number::from_i64(3).into(), list).unwrap();

        assert_eq!(memory.cdr(list).unwrap().tag(), Type::Pair as Tag);
        assert_snapshot!(memory);
    }

    #[test]
    fn convert_false() {
        let memory = Memory::new(create_heap()).unwrap();

        assert_eq!(
            Value::from(memory.boolean(false).unwrap())
                .to_cons()
                .unwrap(),
            memory.boolean(false).unwrap()
        );
    }

    #[test]
    fn convert_true() {
        let memory = Memory::new(create_heap()).unwrap();

        assert_eq!(
            Value::from(memory.boolean(true).unwrap())
                .to_cons()
                .unwrap(),
            memory.boolean(true).unwrap()
        );
    }

    #[test]
    fn convert_null() {
        let memory = Memory::new(create_heap()).unwrap();

        assert_eq!(
            Value::from(memory.null().unwrap()).to_cons().unwrap(),
            memory.null().unwrap()
        );
    }

    fn assert_raw_string<H: Heap>(memory: &Memory<H>, mut cons: Cons, string: &str) {
        for character in string.chars() {
            assert_eq!(
                memory.car(cons).unwrap().assume_number().to_i64(),
                character as _
            );
            cons = memory.cdr(cons).unwrap().assume_cons();
        }

        assert_eq!(cons, memory.null().unwrap());
    }

    #[test]
    fn build_string() {
        let mut memory = Memory::new(create_heap()).unwrap();

        let string = memory.build_string("foo").unwrap();

        assert_eq!(memory.car(string).unwrap(), Number::from_i64(3).into());
        assert_eq!(memory.cdr(string).unwrap().tag(), Type::String as _);
        assert_raw_string(&memory, memory.cdr(string).unwrap().assume_cons(), "foo");
    }

    #[test]
    fn format_string() {
        let mut memory = Memory::new(create_heap()).unwrap();

        memory.set_register(memory.null().unwrap());

        memory.write_str("foo").unwrap();

        assert_raw_string(&memory, memory.register(), "foo");
    }

    #[test]
    fn format_two_strings() {
        let mut memory = Memory::new(create_heap()).unwrap();

        memory.set_register(memory.null().unwrap());

        memory.write_str("foo").unwrap();
        memory.write_str("bar").unwrap();

        assert_raw_string(&memory, memory.register(), "foobar");
    }

    #[test]
    fn format_templated_string() {
        const FOO: usize = 42;

        let mut memory = Memory::new(create_heap()).unwrap();

        memory.set_register(memory.null().unwrap());

        write!(&mut memory, "foo{FOO}bar").unwrap();

        assert_raw_string(&memory, memory.register(), "foo42bar");
    }

    mod stack {
        use super::*;

        #[test]
        fn push_and_pop() {
            let mut memory = Memory::new(create_heap()).unwrap();

            memory.stack = memory.null().unwrap();
            memory.push(Number::from_i64(42).into()).unwrap();

            assert_eq!(memory.pop().unwrap(), Number::from_i64(42).into());
        }

        #[test]
        fn push_and_pop_twice() {
            let mut memory = Memory::new(create_heap()).unwrap();

            memory.stack = memory.null().unwrap();
            memory.push(Number::from_i64(1).into()).unwrap();
            memory.push(Number::from_i64(2).into()).unwrap();

            assert_eq!(memory.pop().unwrap(), Number::from_i64(2).into());
            assert_eq!(memory.pop().unwrap(), Number::from_i64(1).into());
        }
    }

    mod garbage_collection {
        use super::*;

        #[test]
        fn collect_cons() {
            let mut memory = Memory::new(create_heap()).unwrap();

            memory
                .allocate(Number::default().into(), Number::default().into())
                .unwrap();
            memory.collect_garbages(None).unwrap();

            assert_snapshot!(memory);
        }

        #[test]
        fn collect_stack() {
            let mut memory = Memory::new(create_heap()).unwrap();

            memory.stack = memory.null().unwrap();
            memory.push(Number::from_i64(42).into()).unwrap();
            memory.collect_garbages(None).unwrap();

            assert_snapshot!(memory);
        }

        #[test]
        fn collect_deep_stack() {
            let mut memory = Memory::new(create_heap()).unwrap();

            memory.stack = memory.null().unwrap();
            memory.push(Number::from_i64(1).into()).unwrap();
            memory.push(Number::from_i64(2).into()).unwrap();
            memory.collect_garbages(None).unwrap();

            assert_snapshot!(memory);
        }

        #[test]
        fn collect_cycle() {
            let mut memory = Memory::new(create_heap()).unwrap();

            let cons = memory
                .allocate(Number::default().into(), Number::default().into())
                .unwrap();
            memory.set_cdr(cons, cons.into()).unwrap();

            memory.collect_garbages(None).unwrap();

            assert_snapshot!(memory);
        }
    }
}
