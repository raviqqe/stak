mod fixed_buffer;

pub use fixed_buffer::FixedBufferDevice;

pub trait Device {
    type Error;

    fn read(&mut self) -> Result<u8, Self::Error>;
    fn write(&mut self, byte: u8) -> Result<(), Self::Error>;
}
