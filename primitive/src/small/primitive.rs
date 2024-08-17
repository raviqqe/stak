#[repr(u8)]
#[derive(Clone, Copy)]
pub(super) enum Primitive {
    Rib,
    Cons,
    Close,
    IsRib,
    Car,
    Cdr,
    Type,
    Tag,
    SetCar,
    SetCdr,
    Equal,
    LessThan,
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
    Read,
    Write,
    WriteError,
    Halt,
    Null,
    Pair,
    OpenFile,
    CloseFile,
    ReadFile,
    WriteFile,
    DeleteFile,
    ExistsFile,
    CommandLine,
    EnvironmentVariables,
}

impl Primitive {
    pub const RIB: u8 = Self::Rib as _;
    pub const CONS: u8 = Self::Cons as _;
    pub const CLOSE: u8 = Self::Close as _;
    pub const IS_RIB: u8 = Self::IsRib as _;
    pub const CAR: u8 = Self::Car as _;
    pub const CDR: u8 = Self::Cdr as _;
    pub const TYPE: u8 = Self::Type as _;
    pub const TAG: u8 = Self::Tag as _;
    pub const SET_CAR: u8 = Self::SetCar as _;
    pub const SET_CDR: u8 = Self::SetCdr as _;
    pub const EQUAL: u8 = Self::Equal as _;
    pub const LESS_THAN: u8 = Self::LessThan as _;
    pub const ADD: u8 = Self::Add as _;
    pub const SUBTRACT: u8 = Self::Subtract as _;
    pub const MULTIPLY: u8 = Self::Multiply as _;
    pub const DIVIDE: u8 = Self::Divide as _;
    pub const REMAINDER: u8 = Self::Remainder as _;
    pub const READ: u8 = Self::Read as _;
    pub const WRITE: u8 = Self::Write as _;
    pub const WRITE_ERROR: u8 = Self::WriteError as _;
    pub const HALT: u8 = Self::Halt as _;
    pub const NULL: u8 = Self::Null as _;
    pub const PAIR: u8 = Self::Pair as _;
    pub const OPEN_FILE: u8 = Self::OpenFile as _;
    pub const CLOSE_FILE: u8 = Self::CloseFile as _;
    pub const READ_FILE: u8 = Self::ReadFile as _;
    pub const WRITE_FILE: u8 = Self::WriteFile as _;
    pub const DELETE_FILE: u8 = Self::DeleteFile as _;
    pub const EXISTS_FILE: u8 = Self::ExistsFile as _;
    pub const COMMAND_LINE: u8 = Self::CommandLine as _;
    pub const ENVIRONMENT_VARIABLES: u8 = Self::EnvironmentVariables as _;
}
