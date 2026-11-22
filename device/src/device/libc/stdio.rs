use super::{
    Read, Write, buffered_reader::BufferedReader, error::LibcError, line_writer::LineWriter,
};
use rustix::{
    fd::BorrowedFd,
    io::{self, retry_on_intr},
    stdio::{stderr, stdin, stdout},
};

const STDIN_BUFFER_SIZE: usize = 1 << 13;
const STDOUT_BUFFER_SIZE: usize = 1 << 10;

macro_rules! fd {
    ($fd:ident) => {
        // SAFETY: We do not modify the file descriptor.
        #[allow(unused_unsafe)]
        unsafe {
            $fd()
        }
    };
}

/// A buffered stdin.
#[derive(Debug)]
pub struct Stdin {
    reader: BufferedReader<STDIN_BUFFER_SIZE, BorrowedFd<'static>>,
}

impl Stdin {
    /// Creates a stdin.
    pub const fn new() -> Self {
        Self {
            reader: BufferedReader::new(fd!(stdin)),
        }
    }
}

impl Read for Stdin {
    type Error = LibcError;

    fn read(&mut self) -> Result<Option<u8>, Self::Error> {
        self.reader.read().map_err(|_| LibcError::Stdin)
    }
}

impl Default for Stdin {
    fn default() -> Self {
        Self::new()
    }
}

/// A line-buffered stdout.
///
/// Buffered bytes are written out when it is dropped.
#[derive(Debug)]
pub struct Stdout {
    writer: LineWriter<STDOUT_BUFFER_SIZE, BorrowedFd<'static>>,
}

impl Stdout {
    /// Creates a stdout.
    pub const fn new() -> Self {
        Self {
            writer: LineWriter::new(fd!(stdout)),
        }
    }
}

impl Write for Stdout {
    type Error = LibcError;

    fn write(&mut self, byte: u8) -> Result<(), Self::Error> {
        self.writer.write(byte).map_err(|_| LibcError::Stdout)
    }
}

impl Default for Stdout {
    fn default() -> Self {
        Self::new()
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
        if retry_on_intr(|| io::write(fd!(stderr), &[byte])) == Ok(1) {
            Ok(())
        } else {
            Err(LibcError::Stderr)
        }
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
