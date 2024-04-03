use core::fmt::Debug;

pub trait Read {
    type Error: Debug;

    fn read(&mut self) -> Result<Option<u8>, Self::Error>;
}

impl<T: Read> Read for &mut T {
    type Error = T::Error;

    fn read(&mut self) -> Result<Option<u8>, Self::Error> {
        (**self).read()
    }
}
