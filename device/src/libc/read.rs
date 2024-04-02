pub trait Read {
    type Error;

    fn read(&mut self) -> Result<Option<u8>, Self::Error>;
}
