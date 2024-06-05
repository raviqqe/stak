use crate::{Error, FileDescriptor, FileSystem, OpenFlagSet};

pub struct LibcFileSystem {}

impl LibcFileSystem {
    pub const fn new() -> Self {
        Self {}
    }
}

impl FileSystem for LibcFileSystem {
    type Error = Error;

    fn open(&self, path: &[u8], flags: OpenFlagSet) -> Result<FileDescriptor, Self::Error> {
        let descriptor = unsafe { libc::open(path as *const _ as _, flags as _) };

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

        if unsafe { libc::read(descriptor as _, &mut buffer as *mut _ as _, 1) } == 0 {
            Ok(buffer[0])
        } else {
            Err(Error::Read)
        }
    }

    fn write(&self, descriptor: FileDescriptor, byte: u8) -> Result<(), Self::Error> {
        let buffer = [byte];

        if unsafe { libc::write(descriptor as _, &buffer as *const _ as _, 1) } == 0 {
            Ok(())
        } else {
            Err(Error::Write)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;

    #[test]
    fn close() {
        let file_system = LibcFileSystem::new();

        let file = tempfile::NamedTempFile::new().unwrap();
        let descriptor = file_system
            .open(file.path().as_os_str().as_encoded_bytes(), 0)
            .unwrap();
        file_system.close(descriptor).unwrap();
    }

    #[test]
    fn read() {
        let file_system = LibcFileSystem::new();

        let mut file = tempfile::NamedTempFile::new().unwrap();

        file.write(b"a").unwrap();

        let descriptor = file_system
            .open(file.path().as_os_str().as_encoded_bytes(), 0)
            .unwrap();
        file_system.read(descriptor).unwrap();
    }

    #[test]
    fn write() {
        let file_system = LibcFileSystem::new();

        let mut file = tempfile::NamedTempFile::new().unwrap();

        file.write(b"a").unwrap();

        let descriptor = file_system
            .open(file.path().as_os_str().as_encoded_bytes(), 0)
            .unwrap();
        file_system.write(descriptor, 42).unwrap();
    }
}
