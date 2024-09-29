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
    pub(super) const READ: usize = Self::Read as _;
    pub(super) const WRITE: usize = Self::Write as _;
    pub(super) const WRITE_ERROR: usize = Self::WriteError as _;
}
