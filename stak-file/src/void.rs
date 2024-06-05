pub struct VoidFileSystem {}

impl FileSystem<Error> for VoidFileSystem {
    fn open(&self, path: &str) -> Result<Box<dyn Read>, Error> {
        Err(Error::new(ErrorKind::NotFound, "File not found"))
    }

    fn read(&self, file: &mut dyn Read, buffer: &mut [u8]) -> Result<usize, Error> {
        Err(Error::new(ErrorKind::NotFound, "File not found"))
    }

    fn write(&self, byte: u8) -> Result<void, Error> {
        Err(Error::new(ErrorKind::NotFound, "File not found"))
    }
}
