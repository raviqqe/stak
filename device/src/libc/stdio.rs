use crate::Device;

pub struct StdioDevice;

impl StdioDevice {
    fn write(fd: i32, byte: u8) -> Result<(), ()> {
        let bytes = [byte];

        if unsafe { libc::write(fd, &bytes as *const _ as _, 1) } == 1 {
            Ok(())
        } else {
            Err(())
        }
    }
}

impl Device for StdioDevice {
    type Error = ();

    fn read(&mut self) -> Result<Option<u8>, Self::Error> {
        let bytes = [0];

        Ok(
            if unsafe { libc::read(libc::STDIN_FILENO, &bytes as *const _ as _, 1) } == 1 {
                Some(bytes[0])
            } else {
                None
            },
        )
    }

    fn write(&mut self, byte: u8) -> Result<(), Self::Error> {
        Self::write(libc::STDOUT_FILENO, byte)
    }

    fn write_error(&mut self, byte: u8) -> Result<(), Self::Error> {
        Self::write(libc::STDERR_FILENO, byte)
    }
}
