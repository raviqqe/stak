use crate::{
    cons::{Cons, Tag, NEVER},
    number::Number,
    r#type::Type,
    value::Value,
    Error,
};
use core::fmt::{self, Display, Formatter};

const CONS_FIELD_COUNT: usize = 2;

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

/// A memory on a virtual machine.
pub struct Memory<'a> {
    code: Cons,
    stack: Cons,
    r#false: Cons,
    register: Cons,
    allocation_index: usize,
    space: bool,
    heap: &'a mut [Value],
}

impl<'a> Memory<'a> {
    /// Creates a memory.
    pub fn new(heap: &'a mut [Value]) -> Result<Self, Error> {
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

    /// Returns a code.
    pub const fn code(&self) -> Cons {
        self.code
    }

    /// Sets a code.
    pub fn set_code(&mut self, value: Cons) {
        self.code = value;
    }

    /// Returns a register.
    pub const fn register(&self) -> Cons {
        self.register
    }

    /// Sets a register.
    pub fn set_register(&mut self, value: Cons) {
        self.register = value;
    }

    /// Returns a stack.
    pub const fn stack(&self) -> Cons {
        self.stack
    }

    /// Sets a stack.
    pub fn set_stack(&mut self, value: Cons) {
        self.stack = value;
    }

    /// Returns a boolean value.
    pub fn boolean(&self, value: bool) -> Cons {
        if value {
            self.cdr(self.r#false).assume_cons()
        } else {
            self.r#false
        }
    }

    /// Returns a null value.
    pub fn null(&self) -> Cons {
        self.car(self.r#false).assume_cons()
    }

    /// Sets a false value.
    pub fn set_false(&mut self, cons: Cons) {
        self.r#false = cons;
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

    /// Pops values from a stack.
    pub fn pop_many<const M: usize>(&mut self) -> [Value; M] {
        let mut values = [Default::default(); M];

        for index in 0..M - 1 {
            values[M - 1 - index] = self.pop();
        }

        values[0] = self.pop();

        values
    }

    /// Pops numbers from a stack.
    pub fn pop_numbers<const M: usize>(&mut self) -> [Number; M] {
        let mut numbers = [Default::default(); M];

        for (index, value) in self.pop_many::<M>().into_iter().enumerate() {
            numbers[index] = value.assume_number();
        }

        numbers
    }

    /// Peeks a value at the top of a stack.
    pub fn top(&mut self) -> Value {
        debug_assert_ne!(self.stack, self.null());

        self.car(self.stack)
    }

    /// Allocates a cons.
    pub fn cons(&mut self, car: Value, cdr: Cons) -> Result<Cons, Error> {
        self.allocate(car, cdr.set_tag(Type::Pair as Tag).into())
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
            return Err(Error::OutOfMemory);
        }

        let cons = Cons::new(self.allocation_end() as u64);
        self.allocation_index += CONS_FIELD_COUNT;

        assert_heap_cons!(self, cons);

        self.set_raw_car(cons, car);
        self.set_raw_cdr(cons, cdr);

        debug_assert!(self.allocation_index <= self.space_size());

        Ok(cons)
    }

    const fn is_out_of_memory(&self) -> bool {
        self.allocation_index >= self.space_size()
    }

    const fn space_size(&self) -> usize {
        self.heap.len() / 2
    }

    const fn allocation_start(&self) -> usize {
        if self.space {
            self.space_size()
        } else {
            0
        }
    }

    const fn allocation_end(&self) -> usize {
        self.allocation_start() + self.allocation_index
    }

    fn at(&self, index: usize) -> Value {
        unsafe { *self.heap.as_ptr().add(index) }
    }

    fn at_mut(&mut self, index: usize) -> &mut Value {
        unsafe { &mut *self.heap.as_mut_ptr().add(index) }
    }

    fn get(&self, index: usize) -> Value {
        assert_heap_access!(self, index);
        self.at(index)
    }

    fn set(&mut self, index: usize, value: Value) {
        assert_heap_access!(self, index);
        *self.at_mut(index) = value
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
        self.at(cons.index())
    }

    fn unchecked_cdr(&self, cons: Cons) -> Value {
        self.at(cons.index() + 1)
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
        self.set(cons.index() + index, value);
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
        *self.at_mut(cons.index()) = value
    }

    fn set_unchecked_cdr(&mut self, cons: Cons, value: Value) {
        *self.at_mut(cons.index() + 1) = value;
    }

    /// Sets a value to a `car` field in a value assumed as a cons.
    pub fn set_car_value(&mut self, cons: Value, value: Value) {
        self.set_car(cons.assume_cons(), value);
    }

    /// Sets a value to a `cdr` field in a value assumed as a cons.
    pub fn set_cdr_value(&mut self, cons: Value, value: Value) {
        self.set_cdr(cons.assume_cons(), value);
    }

    /// Returns a tail of a list.
    pub fn tail(&self, mut list: Cons, mut index: usize) -> Cons {
        while index > 0 {
            list = self.cdr(list).assume_cons();
            index -= 1;
        }

        list
    }

    // Garbage collection

    fn collect_garbages(&mut self, cons: Option<&mut Cons>) -> Result<(), Error> {
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
            let value = self.copy_value(self.get(index))?;
            self.set(index, value);
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

            // Set a forward pointer.
            self.set_unchecked_car(cons, NEVER.into());
            self.set_unchecked_cdr(cons, copy.into());

            copy
        }
        .set_tag(cons.tag()))
    }
}

impl Display for Memory<'_> {
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
                self.car(cons),
                self.cdr(cons)
            )?;

            if index == self.code.index() {
                write!(formatter, " <- code")?;
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
        let memory = Memory::new(&mut heap).unwrap();

        assert_snapshot!(memory);
    }

    #[test]
    fn create_list() {
        let mut heap = create_heap();
        let mut memory = Memory::new(&mut heap).unwrap();

        let list = memory
            .cons(Number::from_i64(1).into(), memory.null())
            .unwrap();

        assert_eq!(memory.cdr(list).tag(), Type::Pair as Tag);
        assert_snapshot!(memory);

        let list = memory.cons(Number::from_i64(2).into(), list).unwrap();

        assert_eq!(memory.cdr(list).tag(), Type::Pair as Tag);
        assert_snapshot!(memory);

        let list = memory.cons(Number::from_i64(3).into(), list).unwrap();

        assert_eq!(memory.cdr(list).tag(), Type::Pair as Tag);
        assert_snapshot!(memory);
    }

    #[test]
    fn convert_false() {
        let mut heap = create_heap();
        let memory = Memory::new(&mut heap).unwrap();

        assert_eq!(
            Value::from(memory.boolean(false)).to_cons().unwrap(),
            memory.boolean(false)
        );
    }

    #[test]
    fn convert_true() {
        let mut heap = create_heap();
        let memory = Memory::new(&mut heap).unwrap();

        assert_eq!(
            Value::from(memory.boolean(true)).to_cons().unwrap(),
            memory.boolean(true)
        );
    }

    #[test]
    fn convert_null() {
        let mut heap = create_heap();
        let memory = Memory::new(&mut heap).unwrap();

        assert_eq!(Value::from(memory.null()).to_cons().unwrap(), memory.null());
    }

    mod stack {
        use super::*;

        #[test]
        fn push_and_pop() {
            let mut heap = create_heap();
            let mut memory = Memory::new(&mut heap).unwrap();

            memory.stack = memory.null();
            memory.push(Number::from_i64(42).into()).unwrap();

            assert_eq!(memory.pop(), Number::from_i64(42).into());
        }

        #[test]
        fn push_and_pop_twice() {
            let mut heap = create_heap();
            let mut memory = Memory::new(&mut heap).unwrap();

            memory.stack = memory.null();
            memory.push(Number::from_i64(1).into()).unwrap();
            memory.push(Number::from_i64(2).into()).unwrap();

            assert_eq!(memory.pop(), Number::from_i64(2).into());
            assert_eq!(memory.pop(), Number::from_i64(1).into());
        }
    }

    mod garbage_collection {
        use super::*;

        #[test]
        fn collect_cons() {
            let mut heap = create_heap();
            let mut memory = Memory::new(&mut heap).unwrap();

            memory
                .allocate(Number::default().into(), Number::default().into())
                .unwrap();
            memory.collect_garbages(None).unwrap();

            assert_snapshot!(memory);
        }

        #[test]
        fn collect_stack() {
            let mut heap = create_heap();
            let mut memory = Memory::new(&mut heap).unwrap();

            memory.stack = memory.null();
            memory.push(Number::from_i64(42).into()).unwrap();
            memory.collect_garbages(None).unwrap();

            assert_snapshot!(memory);
        }

        #[test]
        fn collect_deep_stack() {
            let mut heap = create_heap();
            let mut memory = Memory::new(&mut heap).unwrap();

            memory.stack = memory.null();
            memory.push(Number::from_i64(1).into()).unwrap();
            memory.push(Number::from_i64(2).into()).unwrap();
            memory.collect_garbages(None).unwrap();

            assert_snapshot!(memory);
        }

        #[test]
        fn collect_cycle() {
            let mut heap = create_heap();
            let mut memory = Memory::new(&mut heap).unwrap();

            let cons = memory
                .allocate(Number::default().into(), Number::default().into())
                .unwrap();
            memory.set_cdr(cons, cons.into());

            memory.collect_garbages(None).unwrap();

            assert_snapshot!(memory);
        }
    }
}
