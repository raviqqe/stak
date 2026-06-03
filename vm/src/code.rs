pub const INTEGER_BASE: u128 = 1 << 6;
pub const NUMBER_BASE: u128 = 1 << 4;
pub const TAG_BASE: u128 = 1 << 4;
pub const SHARE_BASE: u128 = (1 << 5) - 1;

/// A byte that no other operation produces, prefixing cyclic encoding
/// operations. Other operations produce only `0`, even values up to `124`, and
/// odd values up to `127`.
pub const CYCLIC_MARKER: u8 = 126;
