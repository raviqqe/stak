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
    /// A primitive code for read.
    pub const READ: u8 = Self::Read as _;
    /// A primitive code for write.
    pub const WRITE: u8 = Self::Write as _;
    /// A primitive code for write error.
    pub const WRITE_ERROR: u8 = Self::WriteError as _;
}
