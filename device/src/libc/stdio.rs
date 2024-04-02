use super::error::Error;
use crate::Device;

#[derive(Clone, Copy, Debug, Default)]
pub struct StdioDevice;

impl StdioDevice {
    pub fn new() -> Self {
        Self
    }

    fn write(fd: i32, byte: u8, error: Error) -> Result<(), Error> {
        let mut bytes = [byte];

        if unsafe { libc::write(fd, &mut bytes as *mut _ as _, 1) } == 1 {
            Ok(())
        } else {
            Err(error)
        }
    }
}

impl Device for StdioDevice {
    type Error = Error;

    fn read(&mut self) -> Result<Option<u8>, Self::Error> {
        let mut bytes = [0];

        Ok(
            if unsafe { libc::read(libc::STDIN_FILENO, &mut bytes as *mut _ as _, 1) } == 1 {
                Some(bytes[0])
            } else {
                None
            },
        )
    }

    fn write(&mut self, byte: u8) -> Result<(), Self::Error> {
        Self::write(libc::STDOUT_FILENO, byte, Error::Stdout)
    }

    fn write_error(&mut self, byte: u8) -> Result<(), Self::Error> {
        Self::write(libc::STDERR_FILENO, byte, Error::Stderr)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn write_to_stdout() {
        StdioDevice::new().write(42).unwrap();
    }

    #[test]
    fn write_to_stderr() {
        StdioDevice::new().write_error(42).unwrap();
    }
}
