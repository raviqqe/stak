#[derive(Clone, Copy)]
pub enum Primitive {
    Cons,
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

impl Primitive {
    pub const CONS: u8 = Self::Cons as u8;
    pub const ID: u8 = Self::Id as u8;
    pub const POP: u8 = Self::Pop as u8;
    pub const SKIP: u8 = Self::Skip as u8;
    pub const CLOSE: u8 = Self::Close as u8;
    pub const IS_RIB: u8 = Self::IsRib as u8;
    pub const FIELD0: u8 = Self::Field0 as u8;
    pub const FIELD1: u8 = Self::Field1 as u8;
    pub const FIELD2: u8 = Self::Field2 as u8;
    pub const SET_FIELD0: u8 = Self::SetField0 as u8;
    pub const SET_FIELD1: u8 = Self::SetField1 as u8;
    pub const SET_FIELD2: u8 = Self::SetField2 as u8;
    pub const EQUAL: u8 = Self::Equal as u8;
    pub const LESS_THAN: u8 = Self::LessThan as u8;
    pub const ADD: u8 = Self::Add as u8;
    pub const SUBTRACT: u8 = Self::Subtract as u8;
    pub const MULTIPLY: u8 = Self::Multiply as u8;
    pub const DIVIDE: u8 = Self::Divide as u8;
    pub const GET_C: u8 = Self::GetC as u8;
    pub const PUT_C: u8 = Self::PutC as u8;
}
