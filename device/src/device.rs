pub trait Device {
    type Error: Debug;

    fn read(&mut self) -> Result<u8, Self::Error>;
    fn write(&mut self, byte: u8) -> Result<(), Self::Error>;

    fn write_error(&mut self, byte: u8) -> Result<(), Self::Error> {
        self.write(byte)
    }
}
