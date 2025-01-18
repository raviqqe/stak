use super::{utility::decode_path, FileDescriptor, FileError, FileSystem};
use core::ffi::CStr;
use heapless::{FnvIndexMap, Vec};
use rustix::{
    fd::{AsFd, BorrowedFd, OwnedFd},
    fs::{self, Access, Mode, OFlags},
    io::{self, Errno},
};
use stak_vm::{Memory, Value};

const PATH_SIZE: usize = 128;

pub struct CString(Vec<u8, PATH_SIZE>);

impl CString {
    const fn new(vector: Vec<u8, PATH_SIZE>) -> Self {
        Self(vector)
    }
}

impl AsRef<CStr> for CString {
    fn as_ref(&self) -> &CStr {
        CStr::from_bytes_with_nul(&self.0).expect("null-terminated string")
    }
}

/// A file system based on the libc API.
#[derive(Debug, Default)]
pub struct LibcFileSystem {
    descriptor: FileDescriptor,
    files: FnvIndexMap<FileDescriptor, OwnedFd, 32>,
}

impl LibcFileSystem {
    /// Creates a file system.
    pub fn new() -> Self {
        Self::default()
    }

    fn file(&mut self, descriptor: FileDescriptor) -> BorrowedFd {
        self.files[&descriptor].as_fd()
    }
}

impl FileSystem for LibcFileSystem {
    type Path = CStr;
    type PathBuf = CString;
    type Error = FileError;

    fn open(&mut self, path: &Self::Path, output: bool) -> Result<FileDescriptor, Self::Error> {
        let file = fs::open(
            path,
            if output {
                // spell-checker: disable-next-line
                OFlags::WRONLY | OFlags::CREATE | OFlags::TRUNC
            } else {
                // spell-checker: disable-next-line
                OFlags::RDONLY
            },
            // spell-checker: disable-next-line
            Mode::RUSR | Mode::WUSR,
        )
        .map_err(|_| FileError::Open)?;

        let descriptor = self.descriptor;

        self.files
            .insert(descriptor, file)
            .map_err(|_| FileError::Open)?;
        self.descriptor = self.descriptor.wrapping_add(1);

        Ok(descriptor)
    }

    fn close(&mut self, descriptor: FileDescriptor) -> Result<(), Self::Error> {
        self.files.remove(&descriptor);

        Ok(())
    }

    fn read(&mut self, descriptor: FileDescriptor) -> Result<u8, Self::Error> {
        let mut buffer = [0u8; 1];

        io::read(self.file(descriptor), &mut buffer).map_err(|_| FileError::Read)?;

        Ok(buffer[0])
    }

    fn write(&mut self, descriptor: FileDescriptor, byte: u8) -> Result<(), Self::Error> {
        let buffer = [byte];

        io::write(self.file(descriptor), &buffer).map_err(|_| FileError::Write)?;

        Ok(())
    }

    fn delete(&mut self, path: &Self::Path) -> Result<(), Self::Error> {
        fs::unlink(path).map_err(|_| FileError::Delete)
    }

    fn exists(&self, path: &Self::Path) -> Result<bool, Self::Error> {
        match fs::access(path, Access::EXISTS) {
            Ok(()) => Ok(true),
            Err(number) if number == Errno::ACCESS => Ok(false),
            Err(_) => Err(FileError::Exists),
        }
    }

    fn decode_path(memory: &Memory, list: Value) -> Result<Self::PathBuf, Self::Error> {
        let mut path = decode_path::<PATH_SIZE>(memory, list).ok_or(FileError::PathDecode)?;

        path.push(0).map_err(|_| FileError::PathDecode)?;

        Ok(CString::new(path))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use alloc::ffi::CString;
    use std::{fs, path::Path};

    fn decode_c_str(path: &Path) -> CString {
        CString::new(path.as_os_str().as_encoded_bytes()).unwrap()
    }

    #[test]
    fn close() {
        let directory = tempfile::tempdir().unwrap();
        let path = directory.path().join("foo");
        fs::write(&path, []).unwrap();

        let mut file_system = LibcFileSystem::new();

        let descriptor = file_system.open(&decode_c_str(&path), false).unwrap();
        file_system.close(descriptor).unwrap();
    }

    #[test]
    fn read() {
        let directory = tempfile::tempdir().unwrap();
        let path = directory.path().join("foo");

        let mut file_system = LibcFileSystem::new();

        fs::write(&path, [42]).unwrap();

        let descriptor = file_system.open(&decode_c_str(&path), false).unwrap();

        assert_eq!(file_system.read(descriptor).unwrap(), 42);
    }

    #[test]
    fn write() {
        let directory = tempfile::tempdir().unwrap();
        let path = directory.path().join("foo");

        let mut file_system = LibcFileSystem::new();

        let descriptor = file_system.open(&decode_c_str(&path), true).unwrap();

        file_system.write(descriptor, 42).unwrap();
        file_system.close(descriptor).unwrap();

        let descriptor = file_system.open(&decode_c_str(&path), false).unwrap();
        assert_eq!(file_system.read(descriptor).unwrap(), 42);
        file_system.close(descriptor).unwrap();
    }

    #[test]
    fn delete() {
        let directory = tempfile::tempdir().unwrap();
        let path = directory.path().join("foo");
        fs::write(&path, []).unwrap();

        let mut file_system = LibcFileSystem::new();

        file_system.delete(&decode_c_str(&path)).unwrap();

        assert!(!path.exists());
    }

    #[test]
    fn exists() {
        let directory = tempfile::tempdir().unwrap();
        let path = directory.path().join("foo");
        fs::write(&path, []).unwrap();

        let file_system = LibcFileSystem::new();

        assert!(file_system.exists(&decode_c_str(&path)).unwrap());
    }
}
