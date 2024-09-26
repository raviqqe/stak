mod primitive_set;

pub use primitive_set::FilePrimitiveSet;

/// A primitive of a file system.
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
    const OPEN_FILE: usize = Self::OpenFile as _;
    const CLOSE_FILE: usize = Self::CloseFile as _;
    const READ_FILE: usize = Self::ReadFile as _;
    const WRITE_FILE: usize = Self::WriteFile as _;
    const DELETE_FILE: usize = Self::DeleteFile as _;
    const EXISTS_FILE: usize = Self::ExistsFile as _;
}
