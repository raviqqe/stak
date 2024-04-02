use crate::Device;
use core::fmt::{self, Debug, Formatter};

pub struct MemoryDevice;

impl MemoryDevice {
    fn write(fd: i32, byte: u8, error: MemoryError) -> Result<(), MemoryError> {
        let bytes = [byte];

        if unsafe { libc::write(fd, &bytes as *const _ as _, 1) } == 1 {
            Ok(())
        } else {
            Err(error)
        }
    }
}

impl Device for MemoryDevice {
    type Error = MemoryError;

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
        Self::write(libc::STDOUT_FILENO, byte, MemoryError::Stdout)
    }

    fn write_error(&mut self, byte: u8) -> Result<(), Self::Error> {
        Self::write(libc::STDERR_FILENO, byte, MemoryError::Stderr)
    }
}

pub enum MemoryError {
    Stdout,
    Stderr,
}

impl Debug for MemoryError {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        match self {
            Self::Stdout => write!(formatter, "failed to write stdout"),
            Self::Stderr => write!(formatter, "failed to write stderr"),
        }
    }
}
