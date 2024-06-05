use crate::{Error, FileDescriptor, FileSystem, OpenFlagSet};

pub struct LibcFileSystem {}

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
            Err(Error::Open)
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
