#[derive(Clone, Copy, Debug, Eq, PartialEq)]
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
    Exponentiation,
    Logarithm,
    Halt,
    Null,
    Pair,
    Read = 100,
    Write,
    WriteError,
    OpenFile = 200,
    CloseFile,
    ReadFile,
    WriteFile,
    DeleteFile,
    ExistsFile,
    CommandLine = 300,
    EnvironmentVariables,
}

impl Primitive {
    pub const RIB: usize = Self::Rib as _;
    pub const CONS: usize = Self::Cons as _;
    pub const CLOSE: usize = Self::Close as _;
    pub const IS_RIB: usize = Self::IsRib as _;
    pub const CAR: usize = Self::Car as _;
    pub const CDR: usize = Self::Cdr as _;
    pub const TYPE: usize = Self::Type as _;
    pub const TAG: usize = Self::Tag as _;
    pub const SET_CAR: usize = Self::SetCar as _;
    pub const SET_CDR: usize = Self::SetCdr as _;
    pub const EQUAL: usize = Self::Equal as _;
    pub const LESS_THAN: usize = Self::LessThan as _;
    pub const ADD: usize = Self::Add as _;
    pub const SUBTRACT: usize = Self::Subtract as _;
    pub const MULTIPLY: usize = Self::Multiply as _;
    pub const DIVIDE: usize = Self::Divide as _;
    pub const REMAINDER: usize = Self::Remainder as _;
    pub const EXPONENTIATION: usize = Self::Exponentiation as _;
    pub const LOGARITHM: usize = Self::Logarithm as _;
    pub const HALT: usize = Self::Halt as _;
    pub const NULL: usize = Self::Null as _;
    pub const PAIR: usize = Self::Pair as _;
    pub const READ: usize = Self::Read as _;
    pub const WRITE: usize = Self::Write as _;
    pub const WRITE_ERROR: usize = Self::WriteError as _;
    pub const OPEN_FILE: usize = Self::OpenFile as _;
    pub const CLOSE_FILE: usize = Self::CloseFile as _;
    pub const READ_FILE: usize = Self::ReadFile as _;
    pub const WRITE_FILE: usize = Self::WriteFile as _;
    pub const DELETE_FILE: usize = Self::DeleteFile as _;
    pub const EXISTS_FILE: usize = Self::ExistsFile as _;
    pub const COMMAND_LINE: usize = Self::CommandLine as _;
    pub const ENVIRONMENT_VARIABLES: usize = Self::EnvironmentVariables as _;
}
