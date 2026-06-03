pub const INTEGER_BASE: u128 = 1 << 6;
pub const NUMBER_BASE: u128 = 1 << 4;
pub const TAG_BASE: u128 = 1 << 4;
pub const SHARE_BASE: u128 = (1 << 5) - 1;

// A head byte that escapes into a cyclic operation. The three operation classes
// below 126 (dictionary, rib, and number) saturate every other head byte,
// leaving only the even 126 unused. It is also the largest head byte we can
// spare: the compression layer stores a literal byte `x` as `2 * x`, so a head
// byte must not exceed 127.
pub const CYCLE_HEAD: u8 = 126;
// A cyclic operation that allocates a placeholder rib and registers it in a
// dictionary before its fields are known so its descendants can reference it
// back.
pub const CYCLE_CREATE: u8 = 0;
// A cyclic operation that fills the fields of a placeholder rib registered by
// a preceding creation.
pub const CYCLE_PATCH: u8 = 2;
