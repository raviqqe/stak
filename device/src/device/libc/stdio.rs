use super::{Read, Write, error::LibcError};
use rustix::{
    fd::BorrowedFd,
    io,
    stdio::{stderr, stdin, stdout},
};

macro_rules! fd {
    ($fd:ident) => {
        cfg_elif::expr::feature!(if ("std") {
            $fd()
        } else {
            // SAFETY: We do not modify the file descriptor of `stdin`.
            unsafe { $fd() }
        })
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

fn write(fd: BorrowedFd, byte: u8, error: LibcError) -> Result<(), LibcError> {
    if io::write(fd, &[byte]).map_err(|_| error)? == 1 {
        Ok(())
    } else {
        Err(error)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn write_to_stdout() {
        Stdout::new().write(42).unwrap();
    }

    #[test]
    fn write_to_stderr() {
        Stderr::new().write(42).unwrap();
    }
}
