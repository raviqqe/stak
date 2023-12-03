use crate::{Error, Vm};

pub trait PrimitiveSet: Sized {
    type Error: From<Error>;

    fn operate(vm: &mut Vm<Self>, primitive: u8) -> Result<(), Self::Error>;
}
