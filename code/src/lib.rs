//! Bytecodes for Stak Scheme.

#![no_std]

#[cfg(feature = "alloc")]
extern crate alloc;

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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn mask() {
        assert_eq!(INSTRUCTION_MASK, 0b1111);
    }
}
