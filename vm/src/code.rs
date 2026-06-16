pub const INTEGER_BASE: u128 = 1 << 6;
pub const NUMBER_BASE: u128 = 1 << 4;
pub const TAG_BASE: u128 = 1 << 4;
pub const SHARE_BASE: u128 = (1 << 5) - 1;

/// A head byte that escapes into a sharing operation that announces or fills a
/// placeholder rib. It is the only head byte that the reference, rib, and
/// number operations leave unused.
pub const ESCAPE_HEAD: u8 = 0;
/// An escaped operation that announces a placeholder rib before its fields are
/// known so that its descendants can reference it back.
pub const ANNOUNCE: u8 = 0;
/// An escaped operation that fills the fields of an announced placeholder rib.
pub const FILL: u8 = 2;
