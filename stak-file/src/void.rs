use crate::Error;

pub struct VoidFileSystem {}

impl FileSystem<Error> for VoidFileSystem {
    fn open(&self, path: &str) -> Result<Box<dyn Read>, Error> {
        Err(Error::Open)
    }

    fn read(&self, file: &mut dyn Read, buffer: &mut [u8]) -> Result<usize, Error> {
        Err(Error::Read)
    }

    fn write(&self, byte: u8) -> Result<void, Error> {
        Err(Error::Write)
    }
}
