pub trait Write {
    type Error;

    fn write(&mut self, byte: u8) -> Result<(), Self::Error>;
}
