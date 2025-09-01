use crate::{Exception, Value, memory::Memory};
use winter_maybe_async::maybe_async;

/// A primitive set.
///
/// [`PrimitiveSet`](PrimitiveSet)s provide primitive functionalities, such as
/// arithmetic and I/O, to [`Vm`](super::Vm)s. Each primitive has its own
/// identifier.
pub trait PrimitiveSet: Sized {
    /// An error.
    type Error: Exception;

    /// Runs a primitive on a virtual machine.
    #[maybe_async]
    fn operate<T: AsRef<[Value]> + AsMut<[Value]>>(
        &mut self,
        memory: &mut Memory<T>,
        primitive: usize,
    ) -> Result<(), Self::Error>;
}
