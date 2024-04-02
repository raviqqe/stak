use crate::Device;

pub struct MemoryDevice;

impl Device for MemoryDevice {
    type Error = ();

    fn read(&mut self) -> Result<Option<u8>, Self::Error> {
        todo!()
    }

    fn write(&mut self, byte: u8) -> Result<(), Self::Error> {
        let bytes = [byte];

        if unsafe { libc::write(1, &bytes as *const _ as _, 1) } == 1 {
            Ok(())
        } else {
            Err(())
        }
    }

    fn write_error(&mut self, byte: u8) -> Result<(), Self::Error> {
        todo!()
    }
}
