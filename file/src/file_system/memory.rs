use crate::{FileDescriptor, FileError, FileSystem};

#[derive(Clone, Copy, Debug)]
pub struct MemoryFileEntry {
    file_index: usize,
    offset: usize,
}

/// A in-memory file system.
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

        for (file_index, (file_path, _)) in self.files.iter().enumerate() {
            if path == *file_path {
                for (descriptor, entry) in self.entries.iter_mut().enumerate() {
                    if entry.is_none() {
                        *entry = Some(MemoryFileEntry {
                            file_index,
                            offset: 0,
                        });

                        return Ok(descriptor);
                    }
                }
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

    fn read(&mut self, descriptor: FileDescriptor) -> Result<u8, Self::Error> {
        let entry = &mut self
            .entries
            .get_mut(descriptor)
            .map(Option::as_mut)
            .flatten()
            .ok_or(FileError::Read)?;

        let byte = self
            .files
            .get(entry.file_index)
            .ok_or(FileError::Read)?
            .1
            .get(entry.offset)
            .ok_or(FileError::Read)?;

        entry.offset += 1;

        Ok(*byte)
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

    #[test]
    fn read() {
        let mut entries = [Default::default(); 8];
        let mut system = MemoryFileSystem::new(&[(b"foo", b"bar")], &mut entries);

        let descriptor = system.open(b"foo", false).unwrap();

        assert_eq!(system.read(descriptor).unwrap(), b'b');
        assert_eq!(system.read(descriptor).unwrap(), b'a');
        assert_eq!(system.read(descriptor).unwrap(), b'r');

        system.close(descriptor).unwrap();

        assert!(system.read(descriptor).is_err());
    }

    #[test]
    fn exists() {
        let mut entries = [Default::default(); 8];
        let system = MemoryFileSystem::new(&[(b"foo", b"bar")], &mut entries);

        assert!(system.exists(b"foo").unwrap());
        assert!(!system.exists(b"bar").unwrap());
    }
}
