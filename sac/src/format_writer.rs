use core::fmt;
use stak_device::libc::Write;

/// A writer of formatted strings.
#[derive(Debug)]
pub struct FormatWriter<T: Write> {
    writer: T,
}

impl<T: Write> FormatWriter<T> {
    /// Creates a writer.
    pub const fn new(writer: T) -> Self {
        Self { writer }
    }
}

impl<T: Write> fmt::Write for FormatWriter<T> {
    fn write_str(&mut self, string: &str) -> fmt::Result {
        string
            .bytes()
            .try_for_each(|byte| self.writer.write(byte))
            .map_err(|_| fmt::Error)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use core::fmt::Write as _;
    use stak_device::libc::BufferMut;

    #[test]
    fn write_nothing() {
        write!(FormatWriter::new(BufferMut::new(&mut [])), "").unwrap();
    }

    #[test]
    fn write_string() {
        let mut buffer = [0; 3];

        write!(FormatWriter::new(BufferMut::new(&mut buffer)), "foo").unwrap();

        assert_eq!(&buffer, b"foo");
    }

    #[test]
    fn write_multi_byte_characters() {
        let mut buffer = [0; 6];

        write!(FormatWriter::new(BufferMut::new(&mut buffer)), "λ🙂").unwrap();

        assert_eq!(&buffer, "λ🙂".as_bytes());
    }

    #[test]
    fn write_formatted_string() {
        let mut buffer = [0; 6];

        writeln!(
            FormatWriter::new(BufferMut::new(&mut buffer)),
            "{} {}",
            u8::MIN,
            u8::MAX
        )
        .unwrap();

        assert_eq!(&buffer, b"0 255\n");
    }

    #[test]
    fn fail_to_write() {
        let mut buffer = [0; 2];

        assert_eq!(
            write!(FormatWriter::new(BufferMut::new(&mut buffer)), "foo"),
            Err(fmt::Error)
        );
        assert_eq!(&buffer, b"fo");
    }
}
