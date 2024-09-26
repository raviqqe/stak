mod error;
mod primitive_set;

pub use error::PrimitiveError;
pub use primitive_set::DevicePrimitiveSet;

/// A primitive for a device.
pub enum Primitive {
    /// Read.
    Read,
    /// Write.
    Write,
    /// Write error.
    WriteError,
}

impl Primitive {
    const READ: u8 = Self::Read as _;
    const WRITE: u8 = Self::Write as _;
    const WRITE_ERROR: u8 = Self::WriteError as _;
}
