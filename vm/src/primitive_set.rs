use crate::{Error, Vm};

/// A primitive set.
pub trait PrimitiveSet: Sized {
    type Error: From<Error>;

    /// Runs a primitive on a virtual machine.
    fn operate(vm: &mut Vm<Self>, primitive: u8) -> Result<(), Self::Error>;
}
