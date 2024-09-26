use crate::{memory::Memory, Error, Number, Value};

/// A primitive set.
///
/// [`PrimitiveSet`](PrimitiveSet)s provide primitive functionalities, such as
/// arithmetic and I/O, to [`Vm`](super::Vm)s. Each primitive has its own
/// identifier.
pub trait PrimitiveSet: Sized {
    /// An error.
    type Error: From<Error>;

    /// Runs a primitive on a virtual machine.
    fn operate(&mut self, memory: &mut Memory, primitive: u8) -> Result<(), Self::Error>;

    /// Pops arguments of numbers from a stack.
    fn pop_number_arguments<const M: usize>(memory: &mut Memory) -> [Number; M] {
        let mut numbers = [Default::default(); M];

        for (index, value) in Self::pop_arguments::<M>(memory).into_iter().enumerate() {
            numbers[index] = value.assume_number();
        }

        numbers
    }

    /// Pops arguments from a stack.
    fn pop_arguments<const M: usize>(memory: &mut Memory) -> [Value; M] {
        let mut values = [Default::default(); M];

        for index in 0..M - 1 {
            values[M - 1 - index] = memory.pop();
        }

        values[0] = memory.pop();

        values
    }
}
