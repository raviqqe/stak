use super::utility::decode_path;
use crate::{FileDescriptor, FileSystem};
use core::str;
use stak_vm::{Memory, Value};
use std::{
    collections::HashMap,
    fs::{File, OpenOptions, remove_file},
    io::{self, ErrorKind, Read, Write},
    path::{Path, PathBuf},
};

const PATH_SIZE: usize = 256;

/// A file system on an operating system.
#[derive(Debug, Default)]
pub struct OsFileSystem {
    descriptor: FileDescriptor,
    files: HashMap<FileDescriptor, File>,
}

impl OsFileSystem {
    /// Creates a file system.
    pub fn new() -> Self {
        Self::default()
    }

    fn file_mut(&mut self, descriptor: FileDescriptor) -> Result<&mut File, io::Error> {
        self.files
            .get_mut(&descriptor)
            .ok_or_else(|| io::Error::new(ErrorKind::InvalidData, "corrupted file descriptor"))
    }
}

impl FileSystem for OsFileSystem {
    type Path = Path;
    type PathBuf = PathBuf;
    type Error = io::Error;

    fn open(&mut self, path: &Path, output: bool) -> Result<FileDescriptor, Self::Error> {
        let file = OpenOptions::new()
            .read(!output)
            .create(output)
            .write(output)
            .truncate(output)
            .open(path)?;
        let descriptor = self.descriptor;

        self.files.insert(descriptor, file);
        self.descriptor = self.descriptor.wrapping_add(1);

        Ok(descriptor)
    }

    fn close(&mut self, descriptor: FileDescriptor) -> Result<(), Self::Error> {
        self.files.remove(&descriptor);

        Ok(())
    }

    fn read(&mut self, descriptor: FileDescriptor) -> Result<Option<u8>, Self::Error> {
        let mut buffer = [0u8; 1];
        let count = self.file_mut(descriptor)?.read(&mut buffer)?;

        Ok((count > 0).then_some(buffer[0]))
    }

    fn write(&mut self, descriptor: FileDescriptor, byte: u8) -> Result<(), Self::Error> {
        let file = self.file_mut(descriptor)?;
        file.write_all(&[byte])?;

        Ok(())
    }

    fn flush(&mut self, descriptor: FileDescriptor) -> Result<(), Self::Error> {
        let file = self.file_mut(descriptor)?;
        file.flush()?;

        Ok(())
    }

    fn delete(&mut self, path: &Path) -> Result<(), Self::Error> {
        remove_file(path)
    }

    fn exists(&self, path: &Path) -> Result<bool, Self::Error> {
        Ok(path.exists())
    }

    fn decode_path(memory: &Memory, list: Value) -> Result<Self::PathBuf, Self::Error> {
        Ok(PathBuf::from(
            str::from_utf8(
                &decode_path::<PATH_SIZE>(memory, list)
                    .ok_or_else(|| io::Error::new(ErrorKind::InvalidData, "path too long"))?,
            )
            .map_err(|error| io::Error::new(ErrorKind::InvalidData, error))?,
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn close() {
        let directory = tempfile::tempdir().unwrap();
        let path = directory.path().join("foo");
        fs::write(&path, []).unwrap();

        let mut file_system = OsFileSystem::new();

        let descriptor = file_system.open(&path, false).unwrap();
        file_system.close(descriptor).unwrap();
    }

    #[test]
    fn read() {
        let directory = tempfile::tempdir().unwrap();
        let path = directory.path().join("foo");

        let mut file_system = OsFileSystem::new();

        fs::write(&path, [42]).unwrap();

        let descriptor = file_system.open(&path, false).unwrap();

        assert_eq!(file_system.read(descriptor).unwrap(), 42);
    }

    #[test]
    fn write() {
        let directory = tempfile::tempdir().unwrap();
        let path = directory.path().join("foo");

        let mut file_system = OsFileSystem::new();

        let descriptor = file_system.open(&path, true).unwrap();

        file_system.write(descriptor, 42).unwrap();
        file_system.close(descriptor).unwrap();

        let descriptor = file_system.open(&path, false).unwrap();
        assert_eq!(file_system.read(descriptor).unwrap(), 42);
        file_system.close(descriptor).unwrap();
    }

    #[test]
    fn flush() {
        let directory = tempfile::tempdir().unwrap();
        let path = directory.path().join("foo");

        let mut file_system = OsFileSystem::new();

        let descriptor = file_system.open(&path, true).unwrap();

        file_system.flush(descriptor).unwrap();
        file_system.close(descriptor).unwrap();
    }

    #[test]
    fn delete() {
        let directory = tempfile::tempdir().unwrap();
        let path = directory.path().join("foo");
        fs::write(&path, []).unwrap();

        let mut file_system = OsFileSystem::new();

        file_system.delete(&path).unwrap();

        assert!(!path.exists());
    }

    #[test]
    fn exists() {
        let directory = tempfile::tempdir().unwrap();
        let path = directory.path().join("foo");
        fs::write(&path, []).unwrap();

        let file_system = OsFileSystem::new();

        assert!(file_system.exists(&path).unwrap());
    }
}
