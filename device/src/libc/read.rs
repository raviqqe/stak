pub trait Read {
    type Error;

    fn read(&mut self) -> Result<Option<u8>, Self::Error>;
}

impl<T: Read> Read for &mut T {
    type Error = T::Error;

    fn read(&mut self) -> Result<Option<u8>, Self::Error> {
        (**self).read()
    }
}
