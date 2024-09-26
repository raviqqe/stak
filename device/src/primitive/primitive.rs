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
    pub const READ: u8 = Primitive::Read as _;
    /// A primitive code for write.
    pub const WRITE: u8 = Primitive::Write as _;
    /// A primitive code for write error.
    pub const WRITE_ERROR: u8 = Primitive::WriteError as _;
}
