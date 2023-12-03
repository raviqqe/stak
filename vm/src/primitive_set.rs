use crate::{Error, Vm};

pub trait PrimitiveSet: Sized {
    type Error;

    fn operate(vm: &mut Vm<Self>, primitive: u8) -> Result<(), Error<Self::Error>>;
}
