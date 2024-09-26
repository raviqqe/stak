mod error;
mod primitive_set;

pub use error::PrimitiveError;
pub use primitive_set::DevicePrimitiveSet;

/// A primitive of a device.
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
    const READ: usize = Self::Read as _;
    const WRITE: usize = Self::Write as _;
    const WRITE_ERROR: usize = Self::WriteError as _;
}
