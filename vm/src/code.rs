// A bytecode format version stored as a program's first byte.
pub const VERSION: u8 = 0;

pub const INTEGER_BASE: u128 = 1 << 6;
pub const NUMBER_BASE: u128 = 1 << 4;
pub const TAG_BASE: u128 = 1 << 4;
pub const SHARE_BASE: u128 = (1 << 5) - 1;
