use crate::{Exception, memory::Memory};
use winter_maybe_async::maybe_async;

/// A primitive set.
///
/// [`PrimitiveSet`](PrimitiveSet)s provide primitive functionalities, such as
/// arithmetic and I/O, to [`Vm`](super::Vm)s. Each primitive has its own
/// identifier.
pub trait PrimitiveSet<H>: Sized {
    /// An error.
    type Error: Exception;

    /// Runs a primitive on a virtual machine.
    #[maybe_async]
    fn operate(&mut self, memory: &mut Memory<H>, primitive: usize) -> Result<(), Self::Error>;
}
