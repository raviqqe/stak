use crate::{Error, FileDescriptor, FileSystem};

pub struct LibcFileSystem {}

impl LibcFileSystem {
    pub const fn new() -> Self {
        Self {}
    }
}

impl FileSystem for LibcFileSystem {
    type Error = Error;

    fn open(&self, path: &[u8], output: bool) -> Result<FileDescriptor, Self::Error> {
        let descriptor = unsafe {
            libc::open(
                path as *const _ as _,
                if output {
                    libc::O_WRONLY | libc::O_CREAT | libc::O_TRUNC
                } else {
                    libc::O_RDONLY
                },
            )
        };

        if descriptor >= 0 {
            Ok(descriptor as _)
        } else {
            Err(Error::Open)
        }
    }

    fn close(&self, descriptor: FileDescriptor) -> Result<(), Self::Error> {
        if unsafe { libc::close(descriptor as _) } == 0 {
            Ok(())
        } else {
            Err(Error::Close)
        }
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
        let buffer = [byte];

        if unsafe { libc::write(descriptor as _, &buffer as *const _ as _, 1) } == 1 {
            Ok(())
        } else {
            Err(Error::Write)
        }
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
        fs::write(&path, &[]).unwrap();

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

        fs::write(&path, &[42]).unwrap();

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
}
