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
    pub const CONS: u8 = Self::Cons as _;
    pub const ID: u8 = Self::Id as _;
    pub const POP: u8 = Self::Pop as _;
    pub const SKIP: u8 = Self::Skip as _;
    pub const CLOSE: u8 = Self::Close as _;
    pub const IS_CONS: u8 = Self::IsCons as _;
    pub const CAR: u8 = Self::Car as _;
    pub const CDR: u8 = Self::Cdr as _;
    pub const SET_CAR: u8 = Self::SetCar as _;
    pub const SET_CDR: u8 = Self::SetCdr as _;
    pub const EQUAL: u8 = Self::Equal as _;
    pub const LESS_THAN: u8 = Self::LessThan as _;
    pub const ADD: u8 = Self::Add as _;
    pub const SUBTRACT: u8 = Self::Subtract as _;
    pub const MULTIPLY: u8 = Self::Multiply as _;
    pub const DIVIDE: u8 = Self::Divide as _;
    pub const GET_C: u8 = Self::GetC as _;
    pub const PUT_C: u8 = Self::PutC as _;
}
