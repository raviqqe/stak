use crate::Device;
use core::fmt::{self, Debug, Formatter};

#[derive(Clone, Copy, Debug, Default)]
pub struct StdioDevice;

impl StdioDevice {
    pub fn new() -> Self {
        Self
    }

    fn write(fd: i32, byte: u8, error: StdioError) -> Result<(), StdioError> {
        let bytes = [byte];

        if unsafe { libc::write(fd, &bytes as *const _ as _, 1) } == 1 {
            Ok(())
        } else {
            Err(error)
        }
    }
}

impl Device for StdioDevice {
    type Error = StdioError;

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
        Self::write(libc::STDOUT_FILENO, byte, StdioError::Stdout)
    }

    fn write_error(&mut self, byte: u8) -> Result<(), Self::Error> {
        Self::write(libc::STDERR_FILENO, byte, StdioError::Stderr)
    }
}

pub enum StdioError {
    Stdout,
    Stderr,
}

impl Debug for StdioError {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        match self {
            Self::Stdout => write!(formatter, "failed to write stdout"),
            Self::Stderr => write!(formatter, "failed to write stderr"),
        }
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
