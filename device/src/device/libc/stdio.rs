use super::{Read, Write, error::LibcError};
use core::fmt;
use rustix::{
    fd::BorrowedFd,
    io,
    stdio::{stderr, stdin, stdout},
};

macro_rules! fd {
    ($fd:ident) => {
        // SAFETY: We do not modify the file descriptor.
        #[allow(unused_unsafe)]
        unsafe {
            $fd()
        }
    };
}

/// A stdin.
#[derive(Debug, Default)]
pub struct Stdin {}

impl Stdin {
    /// Creates a stdin.
    pub const fn new() -> Self {
        Self {}
    }
}

impl Read for Stdin {
    type Error = LibcError;

    fn read(&mut self) -> Result<Option<u8>, Self::Error> {
        let mut bytes = [0];

        Ok(
            if io::read(fd!(stdin), &mut bytes).map_err(|_| LibcError::Stdin)? == 1 {
                Some(bytes[0])
            } else {
                None
            },
        )
    }
}

/// A stdout.
#[derive(Debug, Default)]
pub struct Stdout {}

impl Stdout {
    /// Creates a stdout.
    pub const fn new() -> Self {
        Self {}
    }
}

impl Write for Stdout {
    type Error = LibcError;

    fn write(&mut self, byte: u8) -> Result<(), Self::Error> {
        write(fd!(stdout), byte, LibcError::Stdout)
    }
}

impl fmt::Write for Stdout {
    fn write_str(&mut self, string: &str) -> fmt::Result {
        write_string(self, string)
    }
}

/// A stderr.
#[derive(Debug, Default)]
pub struct Stderr {}

impl Stderr {
    /// Creates a stderr.
    pub const fn new() -> Self {
        Self {}
    }
}

impl Write for Stderr {
    type Error = LibcError;

    fn write(&mut self, byte: u8) -> Result<(), Self::Error> {
        write(fd!(stderr), byte, LibcError::Stderr)
    }
}

impl fmt::Write for Stderr {
    fn write_str(&mut self, string: &str) -> fmt::Result {
        write_string(self, string)
    }
}

fn write(fd: BorrowedFd, byte: u8, error: LibcError) -> Result<(), LibcError> {
    if io::write(fd, &[byte]).map_err(|_| error)? == 1 {
        Ok(())
    } else {
        Err(error)
    }
}

// TODO Implement `core::io::Write` instead when it is stabilized.
fn write_string(writer: &mut impl Write, string: &str) -> fmt::Result {
    string
        .bytes()
        .try_for_each(|byte| writer.write(byte))
        .map_err(|_| fmt::Error)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::libc::BufferMut;
    use core::fmt::Write as _;

    #[test]
    fn write_to_stdout() {
        Stdout::new().write(42).unwrap();
    }

    #[test]
    fn write_to_stderr() {
        Stderr::new().write(42).unwrap();
    }

    #[test]
    fn write_string_to_stdout() {
        write!(Stdout::new(), "foo").unwrap();
    }

    #[test]
    fn write_string_to_stderr() {
        write!(Stderr::new(), "foo").unwrap();
    }

    #[test]
    fn write_string_to_buffer() {
        let mut buffer = [0; 3];

        write_string(&mut BufferMut::new(&mut buffer), "foo").unwrap();

        assert_eq!(&buffer, b"foo");
    }

    #[test]
    fn write_multi_byte_characters_to_buffer() {
        let mut buffer = [0; 6];

        write_string(&mut BufferMut::new(&mut buffer), "λ🙂").unwrap();

        assert_eq!(&buffer, "λ🙂".as_bytes());
    }

    #[test]
    fn write_too_long_string_to_buffer() {
        let mut buffer = [0; 2];

        assert_eq!(
            write_string(&mut BufferMut::new(&mut buffer), "foo"),
            Err(fmt::Error)
        );
        assert_eq!(&buffer, b"fo");
    }
}
