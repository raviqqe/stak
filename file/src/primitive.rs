mod error;
mod primitive_set;

pub use error::PrimitiveError;
pub use primitive_set::FilePrimitiveSet;

/// A primitive of a file system.
#[repr(u8)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Primitive {
    /// Open a file.
    OpenFile,
    /// Close a file.
    CloseFile,
    /// Read a file.
    ReadFile,
    /// Write a file.
    WriteFile,
    /// Delete a file.
    DeleteFile,
    /// Check if a file exists.
    ExistsFile,
}

impl Primitive {
    const OPEN_FILE: u8 = Self::OpenFile as _;
    const CLOSE_FILE: u8 = Self::CloseFile as _;
    const READ_FILE: u8 = Self::ReadFile as _;
    const WRITE_FILE: u8 = Self::WriteFile as _;
    const DELETE_FILE: u8 = Self::DeleteFile as _;
    const EXISTS_FILE: u8 = Self::ExistsFile as _;
}
