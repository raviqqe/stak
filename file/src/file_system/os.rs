use crate::{FileDescriptor, FileSystem};
use core::{ffi::CStr, mem::forget};
use std::{
    ffi::OsStr,
    fs::{remove_file, File, OpenOptions},
    io::{self, Read, Write},
    os::fd::{FromRawFd, IntoRawFd},
    path::PathBuf,
};

/// An OS file system.
#[derive(Debug)]
pub struct OsFileSystem {}

impl OsFileSystem {
    /// Creates a file system.
    pub const fn new() -> Self {
        Self {}
    }

    fn create_path(path: &[u8]) -> PathBuf {
        let string = CStr::from_bytes_with_nul(path).unwrap();
        PathBuf::from(unsafe { OsStr::from_encoded_bytes_unchecked(string.to_bytes()) })
    }
}

impl FileSystem for OsFileSystem {
    type Error = io::Error;

    fn open(&self, path: &[u8], output: bool) -> Result<FileDescriptor, Self::Error> {
        OpenOptions::new()
            .read(!output)
            .create(output)
            .write(output)
            .truncate(output)
            .open(Self::create_path(path))
            .map(|file| file.into_raw_fd() as _)
    }

    fn close(&self, descriptor: FileDescriptor) -> Result<(), Self::Error> {
        unsafe { File::from_raw_fd(descriptor as _) };

        Ok(())
    }

    fn read(&self, descriptor: FileDescriptor) -> Result<u8, Self::Error> {
        let mut file = unsafe { File::from_raw_fd(descriptor as _) };
        let mut buffer = [0u8; 1];
        file.read_exact(&mut buffer)?;
        forget(file);

        Ok(buffer[0])
    }

    fn write(&self, descriptor: FileDescriptor, byte: u8) -> Result<(), Self::Error> {
        let mut file = unsafe { File::from_raw_fd(descriptor as _) };
        file.write_all(&[byte])?;
        forget(file);

        Ok(())
    }

    fn delete(&self, path: &[u8]) -> Result<(), Self::Error> {
        remove_file(Self::create_path(path))
    }

    fn exists(&self, path: &[u8]) -> Result<bool, Self::Error> {
        Ok(Self::create_path(path).exists())
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

        let file_system = OsFileSystem::new();

        let descriptor = file_system
            .open(create_path_string(&path).to_bytes_with_nul(), false)
            .unwrap();
        file_system.close(descriptor).unwrap();
    }

    #[test]
    fn read() {
        let directory = tempfile::tempdir().unwrap();
        let path = directory.path().join("foo");

        let file_system = OsFileSystem::new();

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

        let file_system = OsFileSystem::new();

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

        let file_system = OsFileSystem::new();

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
