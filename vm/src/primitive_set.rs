use crate::{Error, Vm};

/// A primitive set.
///
/// [`PrimitiveSet`](PrimitiveSet)s provide primitive functionalities, such as arithmetic and I/O, to [`Vm`](Vm)s.
/// Each primitive has its own identifier.
pub trait PrimitiveSet: Sized {
    type Error: From<Error>;

    /// Runs a primitive on a virtual machine.
    fn operate(vm: &mut Vm<Self>, primitive: u8) -> Result<(), Self::Error>;
}
