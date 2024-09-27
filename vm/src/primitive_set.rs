use crate::{memory::Memory, Error};
use core::error;

/// A primitive set.
///
/// [`PrimitiveSet`](PrimitiveSet)s provide primitive functionalities, such as
/// arithmetic and I/O, to [`Vm`](super::Vm)s. Each primitive has its own
/// identifier.
pub trait PrimitiveSet: Sized {
    /// An error.
    type Error: From<Error> + error::Error;

    /// Runs a primitive on a virtual machine.
    fn operate(&mut self, memory: &mut Memory, primitive: usize) -> Result<(), Self::Error>;
}
