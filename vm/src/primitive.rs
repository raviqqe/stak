use num_derive::FromPrimitive;
use num_traits::FromPrimitive;

#[derive(Clone, Copy, FromPrimitive)]
pub enum Primitive {
    Rib,
    Id,
    Pop,
    Skip,
    Close,
    IsRib,
    Field0,
    Field1,
    Field2,
    SetField0,
    SetField1,
    SetField2,
    Equal,
    LessThan,
    Add,
    Subtract,
    Multiply,
    Divide,
    GetC,
    PutC,
}

impl TryFrom<u8> for Primitive {
    type Error = ();

    fn try_from(byte: u8) -> Result<Self, ()> {
        Self::from_u8(byte).ok_or(())
    }
}

impl TryFrom<u64> for Primitive {
    type Error = ();

    fn try_from(number: u64) -> Result<Self, ()> {
        Self::from_u64(number).ok_or(())
    }
}
