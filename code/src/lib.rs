//! Bytecodes for Stak Scheme.

#![no_std]

#[cfg(feature = "alloc")]
extern crate alloc;
#[cfg(test)]
extern crate std;

#[cfg(feature = "alloc")]
mod decode;
#[cfg(feature = "alloc")]
mod encode;
#[cfg(feature = "alloc")]
mod error;
mod ir;

#[cfg(feature = "alloc")]
pub use decode::decode;
#[cfg(feature = "alloc")]
pub use encode::encode;
#[cfg(feature = "alloc")]
pub use error::Error;
pub use ir::*;

/// A number of bits required to encode an instruction in bytecodes.
pub const INSTRUCTION_BITS: u64 = 4;
/// A mask for instruction bits in bytecodes.
pub const INSTRUCTION_MASK: u8 = (1 << INSTRUCTION_BITS) - 1;
/// A base for integer encoding in bytecodes.
pub const INTEGER_BASE: u64 = i8::MAX as u64 + 1;
/// A base for short integer encoding in bytecodes.
pub const SHORT_INTEGER_BASE: u64 = 1 << (8 - INSTRUCTION_BITS - 1);

// Those bytes are not used in UTF-8.
/// A symbol separator.
pub const SYMBOL_SEPARATOR: u8 = 0xFE;
/// A symbol terminator.
pub const SYMBOL_TERMINATOR: u8 = 0xFF;

/// Encoding v2
pub mod v2 {
    /// A base for integer encoding in bytecodes.
    pub const INTEGER_BASE: u128 = 1 << 7;
    /// A base for runtime number encoding in bytecodes.
    pub const NUMBER_BASE: u128 = 1 << 6;
    /// A base for tag encoding in bytecodes.
    pub const TAG_BASE: u128 = 1 << 5;
    /// A base for shared node encoding in bytecodes.
    pub const SHARE_BASE: u128 = (1 << 5) - 1;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn mask() {
        assert_eq!(INSTRUCTION_MASK, 0b1111);
    }
}
