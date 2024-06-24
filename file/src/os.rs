use crate::{Error, FileDescriptor, FileSystem};
use core::ffi::c_int;
// spell-checker: disable-next-line
use libc::{F_OK, S_IRUSR, S_IWUSR};

/// A file system based on the libc API.
#[derive(Debug)]
pub struct LibcFileSystem {}

impl OsFileSystem {
    /// Creates a file system.
    pub const fn new() -> Self {
        Self {}
    }

    fn execute(error: Error, callback: impl Fn() -> c_int) -> Result<(), Error> {
        if callback() == 0 {
            Ok(())
        } else {
            Err(error)
        }
    }
}

impl FileSystem for LibcFileSystem {
    type Error = Error;

    fn open(&self, path: &[u8], output: bool) -> Result<FileDescriptor, Self::Error> {
        let descriptor = unsafe {
            libc::open(
                path as *const _ as _,
                if output {
                    // spell-checker: disable-next-line
                    libc::O_WRONLY | libc::O_CREAT | libc::O_TRUNC
                } else {
                    // spell-checker: disable-next-line
                    libc::O_RDONLY
                },
                // spell-checker: disable-next-line
                (S_IRUSR | S_IWUSR) as c_int,
            )
        };

        if descriptor >= 0 {
            Ok(descriptor as _)
        } else {
            Err(Error::Open)
        }
    }

    fn close(&self, descriptor: FileDescriptor) -> Result<(), Self::Error> {
        Self::execute(Error::Close, || unsafe { libc::close(descriptor as _) })
    }

    fn read(&self, descriptor: FileDescriptor) -> Result<u8, Self::Error> {
        let mut buffer = [0u8; 1];

        if unsafe { libc::read(descriptor as _, &mut buffer as *mut _ as _, 1) } == 1 {
            Ok(buffer[0])
        } else {
            Err(Error::Read)
        }
    }

    fn write(&self, descriptor: FileDescriptor, byte: u8) -> Result<(), Self::Error> {
        Self::execute(Error::Write, || {
            let buffer = [byte];
            (unsafe { libc::write(descriptor as _, &buffer as *const _ as _, 1) } != 1) as i32
        })
    }

    fn delete(&self, path: &[u8]) -> Result<(), Self::Error> {
        Self::execute(Error::Delete, || unsafe {
            libc::remove(path as *const _ as _)
        })
    }

    fn exists(&self, path: &[u8]) -> Result<bool, Self::Error> {
        Ok(unsafe { libc::access(path as *const _ as _, F_OK) } == 0)
    }
}

impl Default for LibcFileSystem {
    fn default() -> Self {
        Self::new()
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

        let file_system = LibcFileSystem::new();

        let descriptor = file_system
            .open(path.as_os_str().as_encoded_bytes(), false)
            .unwrap();
        file_system.close(descriptor).unwrap();
    }

    #[test]
    fn read() {
        let directory = tempfile::tempdir().unwrap();
        let path = directory.path().join("foo");

        let file_system = LibcFileSystem::new();

        fs::write(&path, [42]).unwrap();

        let descriptor = file_system
            .open(path.as_os_str().as_encoded_bytes(), false)
            .unwrap();

        assert_eq!(file_system.read(descriptor).unwrap(), 42);
    }

    #[test]
    fn write() {
        let directory = tempfile::tempdir().unwrap();
        let path = directory.path().join("foo");

        let file_system = LibcFileSystem::new();

        let descriptor = file_system
            .open(path.as_os_str().as_encoded_bytes(), true)
            .unwrap();

        file_system.write(descriptor, 42).unwrap();
        file_system.close(descriptor).unwrap();

        let descriptor = file_system
            .open(path.as_os_str().as_encoded_bytes(), false)
            .unwrap();
        assert_eq!(file_system.read(descriptor).unwrap(), 42);
        file_system.close(descriptor).unwrap();
    }

    #[test]
    fn delete() {
        let directory = tempfile::tempdir().unwrap();
        let path = directory.path().join("foo");
        fs::write(&path, []).unwrap();

        let file_system = LibcFileSystem::new();

        file_system
            .delete(path.as_os_str().as_encoded_bytes())
            .unwrap();

        assert!(!path.exists());
    }

    #[test]
    fn exists() {
        let directory = tempfile::tempdir().unwrap();
        let path = directory.path().join("foo");
        fs::write(&path, []).unwrap();

        let file_system = LibcFileSystem::new();

        assert!(file_system
            .exists(path.as_os_str().as_encoded_bytes())
            .unwrap());
    }
}
