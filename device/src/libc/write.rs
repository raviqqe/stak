use core::fmt::Debug;

pub trait Write {
    type Error: Debug;

    fn write(&mut self, byte: u8) -> Result<(), Self::Error>;
}

impl<T: Write> Write for &mut T {
    type Error = T::Error;

    fn write(&mut self, byte: u8) -> Result<(), Self::Error> {
        (**self).write(byte)
    }
}
