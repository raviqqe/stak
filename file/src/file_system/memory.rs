use super::utility::decode_path;
use crate::{FileDescriptor, FileError, FileSystem};
use heapless::Vec;
use stak_vm::{Memory, Value};

const PATH_SIZE: usize = 128;

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

impl FileSystem for MemoryFileSystem<'_> {
    type Path = [u8];
    type PathBuf = Vec<u8, PATH_SIZE>;
    type Error = FileError;

    fn open(&mut self, path: &Self::Path, output: bool) -> Result<FileDescriptor, Self::Error> {
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

    fn read(&mut self, descriptor: FileDescriptor) -> Result<Option<u8>, Self::Error> {
        let entry = &mut self
            .entries
            .get_mut(descriptor)
            .and_then(Option::as_mut)
            .ok_or(FileError::Read)?;

        let Some(&byte) = self
            .files
            .get(entry.file_index)
            .ok_or(FileError::Read)?
            .1
            .get(entry.offset)
        else {
            return Ok(None);
        };

        entry.offset += 1;

        Ok(Some(byte))
    }

    fn write(&mut self, _: FileDescriptor, _: u8) -> Result<(), Self::Error> {
        Err(FileError::Write)
    }

    fn delete(&mut self, _: &Self::Path) -> Result<(), Self::Error> {
        Err(FileError::Delete)
    }

    fn flush(&mut self, _: FileDescriptor) -> Result<(), Self::Error> {
        Err(FileError::Flush)
    }

    fn exists(&self, path: &Self::Path) -> Result<bool, Self::Error> {
        for (file_path, _) in self.files {
            if &path == file_path {
                return Ok(true);
            }
        }

        Ok(false)
    }

    fn decode_path(memory: &Memory, list: Value) -> Result<Self::PathBuf, Self::Error> {
        decode_path(memory, list).ok_or(FileError::PathDecode)
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

        assert_eq!(system.read(descriptor), Ok(Some(b'b')));
        assert_eq!(system.read(descriptor), Ok(Some(b'a')));
        assert_eq!(system.read(descriptor), Ok(Some(b'r')));
        assert_eq!(system.read(descriptor), Ok(None));

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
