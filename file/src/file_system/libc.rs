use super::{utility::decode_path, FileDescriptor, FileError, FileSystem};
use alloc::ffi::CString;
use core::ffi::{c_int, CStr};
// spell-checker: disable-next-line
use libc::{F_OK, S_IRUSR, S_IWUSR};

const PATH_SIZE: usize = 128;

/// A file system based on the libc API.
#[derive(Debug)]
pub struct LibcFileSystem {}

impl LibcFileSystem {
    /// Creates a file system.
    pub const fn new() -> Self {
        Self {}
    }

    fn execute(error: FileError, callback: impl Fn() -> c_int) -> Result<(), FileError> {
        if callback() == 0 {
            Ok(())
        } else {
            Err(error)
        }
    }
}

impl FileSystem for LibcFileSystem {
    type Path = CStr;
    type PathBuf = CString;
    type Error = FileError;

    fn open(&mut self, path: &Self::Path, output: bool) -> Result<FileDescriptor, Self::Error> {
        let descriptor = unsafe {
            libc::open(
                path.as_ptr(),
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
            Err(FileError::Open)
        }
    }

    fn close(&mut self, descriptor: FileDescriptor) -> Result<(), Self::Error> {
        Self::execute(FileError::Close, || unsafe { libc::close(descriptor as _) })
    }

    fn read(&mut self, descriptor: FileDescriptor) -> Result<u8, Self::Error> {
        let mut buffer = [0u8; 1];

        if unsafe { libc::read(descriptor as _, &mut buffer as *mut _ as _, 1) } == 1 {
            Ok(buffer[0])
        } else {
            Err(FileError::Read)
        }
    }

    fn write(&mut self, descriptor: FileDescriptor, byte: u8) -> Result<(), Self::Error> {
        Self::execute(FileError::Write, || {
            let buffer = [byte];
            (unsafe { libc::write(descriptor as _, &buffer as *const _ as _, 1) } != 1) as i32
        })
    }

    fn delete(&mut self, path: &Self::Path) -> Result<(), Self::Error> {
        Self::execute(FileError::Delete, || unsafe {
            libc::remove(path as *const _ as _)
        })
    }

    fn exists(&self, path: &Self::Path) -> Result<bool, Self::Error> {
        Ok(unsafe { libc::access(path as *const _ as _, F_OK) } == 0)
    }

    fn decode_path(
        &self,
        memory: &stak_vm::Memory,
        list: stak_vm::Value,
    ) -> Result<Self::PathBuf, Self::Error> {
        let mut path = decode_path::<PATH_SIZE>(memory, list).ok_or(FileError::PathDecode)?;

        path.push(0).map_err(|_| FileError::PathDecode)?;

        Ok(CStr::from_bytes_with_nul(&path)
            .map_err(|_| FileError::PathDecode)?
            .into())
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

        let mut file_system = LibcFileSystem::new();

        let descriptor = file_system
            .open(path.as_os_str().as_encoded_bytes(), false)
            .unwrap();
        file_system.close(descriptor).unwrap();
    }

    #[test]
    fn read() {
        let directory = tempfile::tempdir().unwrap();
        let path = directory.path().join("foo");

        let mut file_system = LibcFileSystem::new();

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

        let mut file_system = LibcFileSystem::new();

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

        let mut file_system = LibcFileSystem::new();

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

        assert!(file_system.exists(path.as_os_str().as_c_sr()).unwrap());
    }
}
