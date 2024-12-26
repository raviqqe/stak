use crate::{FileDescriptor, FileError, FileSystem};

#[derive(Debug)]
pub struct MemoryFileEntry {
    file_index: usize,
    offset: usize,
}

/// A read-only in-memory file system.
#[derive(Debug)]
pub struct MemoryFileSystem<'a> {
    files: &'a [(&'a [u8], &'a [u8])],
    entries: &'a mut [Option<MemoryFileEntry>],
}

impl<'a> MemoryFileSystem<'a> {
    /// Creates a file system.
    pub const fn new(
        files: &'a [(&'a [u8], &'a [u8])],
        entries: &'a mut [Option<MemoryFileEntry>],
    ) -> Self {
        Self { files, entries }
    }
}

impl<'a> FileSystem for MemoryFileSystem<'a> {
    type Error = FileError;

    fn open(&mut self, path: &[u8], output: bool) -> Result<FileDescriptor, Self::Error> {
        if output {
            return Err(FileError::Open);
        }

        for (file_path, content) in self.files {
            if &path == file_path {
             for entry in  self.entries {
                     Some(entry) = self.entries.get_mut(descriptor) ;
                    return Err(FileError::Close);
                }}

                return Ok();
            }
        }

        Err(FileError::Open)
    }

    fn close(&mut self, descriptor: FileDescriptor) -> Result<(), Self::Error> {
        let Some(entry) = self.entries.get_mut(descriptor) else {
            return Err(FileError::Close);
        };

        *entry = None;

        Ok(())
    }

    fn read(&mut self, path: FileDescriptor) -> Result<u8, Self::Error> {
        Err(FileError::Read)
    }

    fn write(&mut self, _: FileDescriptor, _: u8) -> Result<(), Self::Error> {
        Err(FileError::Write)
    }

    fn delete(&mut self, _: &[u8]) -> Result<(), Self::Error> {
        Err(FileError::Delete)
    }

    fn exists(&self, path: &[u8]) -> Result<bool, Self::Error> {
        for (file_path, _) in self.files {
            if &path == file_path {
                return Ok(true);
            }
        }

        Ok(false)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn create_file_system() {
        MemoryFileSystem::new(&[], &mut []);
    }
}
