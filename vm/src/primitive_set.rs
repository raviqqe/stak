use crate::{Error, Vm};

pub trait PrimitiveSet: Sized {
    fn operate(vm: &mut Vm<Self>, operation: u8) -> Result<(), Error>;
}
