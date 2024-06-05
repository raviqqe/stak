use super::{error::Error, Read, Write};

#[derive(Debug, Default)]
pub struct Stdin {}

impl Stdin {
    pub const fn new() -> Self {
        Self {}
    }
}

impl Read for Stdin {
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
}

#[derive(Debug, Default)]
pub struct Stdout {}

impl Stdout {
    pub const fn new() -> Self {
        Self {}
    }
}

impl Write for Stdout {
    type Error = Error;

    fn write(&mut self, byte: u8) -> Result<(), Self::Error> {
        write(libc::STDOUT_FILENO, byte, Error::Stdout)
    }
}

#[derive(Debug, Default)]
pub struct Stderr {}

impl Stderr {
    pub const fn new() -> Self {
        Self {}
    }
}

impl Write for Stderr {
    type Error = Error;

    fn write(&mut self, byte: u8) -> Result<(), Self::Error> {
        write(libc::STDERR_FILENO, byte, Error::Stderr)
    }
}

fn write(fd: i32, byte: u8, error: Error) -> Result<(), Error> {
    let mut bytes = [byte];

    if unsafe { libc::write(fd, &mut bytes as *mut _ as _, 1) } == 1 {
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
