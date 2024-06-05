use crate::{Error, FileDescriptor, OpenFlagSet};
use core::fmt::Debug;

pub trait FileSystem {
    type Error: Debug;

    fn open(&self, path: &[u8], flags: OpenFlagSet) -> Result<FileDescriptor, Error>;
    fn close(&self, descriptor: FileDescriptor) -> Result<(), Error>;
    fn read(&self, descriptor: FileDescriptor) -> Result<u8, Error>;
    fn write(&self, descriptor: FileDescriptor, byte: u8) -> Result<(), Error>;
}
