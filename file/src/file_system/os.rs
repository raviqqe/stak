use super::utility::decode_path;
use crate::{FileDescriptor, FileSystem};
use core::{mem::forget, str};
use std::{
    fs::{remove_file, File, OpenOptions},
    io::{self, ErrorKind, Read, Write},
    os::fd::{FromRawFd, IntoRawFd},
    path::{Path, PathBuf},
};

const PATH_SIZE: usize = 256;

/// A file system on an operating system.
#[derive(Debug)]
pub struct OsFileSystem {}

impl OsFileSystem {
    /// Creates a file system.
    pub const fn new() -> Self {
        Self {}
    }
}

impl FileSystem for OsFileSystem {
    type Path = Path;
    type PathBuf = PathBuf;
    type Error = io::Error;

    fn open(&mut self, path: &Path, output: bool) -> Result<FileDescriptor, Self::Error> {
        OpenOptions::new()
            .read(!output)
            .create(output)
            .write(output)
            .truncate(output)
            .open(path)
            .map(|file| file.into_raw_fd() as _)
    }

    fn close(&mut self, descriptor: FileDescriptor) -> Result<(), Self::Error> {
        unsafe { File::from_raw_fd(descriptor as _) };

        Ok(())
    }

    fn read(&mut self, descriptor: FileDescriptor) -> Result<u8, Self::Error> {
        let mut file = unsafe { File::from_raw_fd(descriptor as _) };
        let mut buffer = [0u8; 1];
        file.read_exact(&mut buffer)?;
        forget(file);

        Ok(buffer[0])
    }

    fn write(&mut self, descriptor: FileDescriptor, byte: u8) -> Result<(), Self::Error> {
        let mut file = unsafe { File::from_raw_fd(descriptor as _) };
        file.write_all(&[byte])?;
        forget(file);

        Ok(())
    }

    fn delete(&mut self, path: &Path) -> Result<(), Self::Error> {
        remove_file(path)
    }

    fn exists(&self, path: &Path) -> Result<bool, Self::Error> {
        Ok(path.exists())
    }

    fn decode_path(
        memory: &stak_vm::Memory,
        list: stak_vm::Value,
    ) -> Result<Self::PathBuf, Self::Error> {
        Ok(PathBuf::from(
            str::from_utf8(
                &decode_path::<PATH_SIZE>(memory, list)
                    .ok_or_else(|| io::Error::new(ErrorKind::InvalidData, "path decode failed"))?,
            )
            .map_err(|error| io::Error::new(ErrorKind::InvalidData, error))?,
        ))
    }
}

impl Default for OsFileSystem {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use alloc::ffi::CString;
    use std::{fs, path::Path};

    fn create_path_string(path: &Path) -> CString {
        CString::new(path.as_os_str().as_encoded_bytes()).unwrap()
    }

    #[test]
    fn close() {
        let directory = tempfile::tempdir().unwrap();
        let path = directory.path().join("foo");
        fs::write(&path, []).unwrap();

        let mut file_system = OsFileSystem::new();

        let descriptor = file_system
            .open(create_path_string(&path).to_bytes_with_nul(), false)
            .unwrap();
        file_system.close(descriptor).unwrap();
    }

    #[test]
    fn read() {
        let directory = tempfile::tempdir().unwrap();
        let path = directory.path().join("foo");

        let mut file_system = OsFileSystem::new();

        fs::write(&path, [42]).unwrap();

        let descriptor = file_system
            .open(create_path_string(&path).to_bytes_with_nul(), false)
            .unwrap();

        assert_eq!(file_system.read(descriptor).unwrap(), 42);
    }

    #[test]
    fn write() {
        let directory = tempfile::tempdir().unwrap();
        let path = directory.path().join("foo");

        let mut file_system = OsFileSystem::new();

        let descriptor = file_system
            .open(create_path_string(&path).to_bytes_with_nul(), true)
            .unwrap();

        file_system.write(descriptor, 42).unwrap();
        file_system.close(descriptor).unwrap();

        let descriptor = file_system
            .open(create_path_string(&path).to_bytes_with_nul(), false)
            .unwrap();
        assert_eq!(file_system.read(descriptor).unwrap(), 42);
        file_system.close(descriptor).unwrap();
    }

    #[test]
    fn delete() {
        let directory = tempfile::tempdir().unwrap();
        let path = directory.path().join("foo");
        fs::write(&path, []).unwrap();

        let mut file_system = OsFileSystem::new();

        file_system
            .delete(create_path_string(&path).to_bytes_with_nul())
            .unwrap();

        assert!(!path.exists());
    }

    #[test]
    fn exists() {
        let directory = tempfile::tempdir().unwrap();
        let path = directory.path().join("foo");
        fs::write(&path, []).unwrap();

        let file_system = OsFileSystem::new();

        assert!(file_system
            .exists(create_path_string(&path).to_bytes_with_nul())
            .unwrap());
    }
}
