#[repr(u8)]
#[derive(Clone, Copy)]
pub enum Primitive {
    Cons,
    Id,
    Pop,
    Skip,
    Close,
    IsCons,
    Car,
    Cdr,
    SetCar,
    SetCdr,
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
    pub const IS_CONS: u8 = Self::IsCons as u8;
    pub const CAR: u8 = Self::Car as u8;
    pub const CDR: u8 = Self::Cdr as u8;
    pub const SET_CAR: u8 = Self::SetCar as u8;
    pub const SET_CDR: u8 = Self::SetCdr as u8;
    pub const EQUAL: u8 = Self::Equal as u8;
    pub const LESS_THAN: u8 = Self::LessThan as u8;
    pub const ADD: u8 = Self::Add as u8;
    pub const SUBTRACT: u8 = Self::Subtract as u8;
    pub const MULTIPLY: u8 = Self::Multiply as u8;
    pub const DIVIDE: u8 = Self::Divide as u8;
    pub const GET_C: u8 = Self::GetC as u8;
    pub const PUT_C: u8 = Self::PutC as u8;
}
