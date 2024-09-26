mod error;
mod primitive_set;

pub use error::PrimitiveError;
pub use primitive_set::DevicePrimitiveSet;

/// A primitive of a device.
#[repr(u8)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Primitive {
    /// Read from a device.
    Read,
    /// Write to a device.
    Write,
    /// Write error to a device.
    WriteError,
}

impl Primitive {
    const READ: u8 = Self::Read as _;
    const WRITE: u8 = Self::Write as _;
    const WRITE_ERROR: u8 = Self::WriteError as _;
}
