use super::sink::Sink;
use rustix::io::{self, Errno, retry_on_intr};

#[derive(Debug)]
pub struct LineWriter<const N: usize, S: Sink> {
    sink: S,
    buffer: [u8; N],
    length: usize,
}

impl<const N: usize, S: Sink> LineWriter<N, S> {
    pub const fn new(sink: S) -> Self {
        Self {
            sink,
            buffer: [0; N],
            length: 0,
        }
    }

    pub fn write(&mut self, byte: u8) -> io::Result<()> {
        if self.length == N {
            self.flush()?;
        }

        self.buffer[self.length] = byte;
        self.length += 1;

        if byte == b'\n' { self.flush() } else { Ok(()) }
    }

    fn flush(&mut self) -> io::Result<()> {
        while self.length > 0 {
            let count = retry_on_intr(|| self.sink.write(&self.buffer[..self.length]))?;

            if count == 0 {
                return Err(Errno::IO);
            }

            self.buffer.copy_within(count..self.length, 0);
            self.length -= count;
        }

        Ok(())
    }
}

impl<const N: usize, S: Sink> Drop for LineWriter<N, S> {
    fn drop(&mut self) {
        let _ = self.flush();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use alloc::{collections::VecDeque, vec, vec::Vec};
    use core::cell::RefCell;

    struct FakeSink {
        results: RefCell<VecDeque<io::Result<usize>>>,
        bytes: RefCell<Vec<u8>>,
    }

    impl FakeSink {
        fn new(results: &[io::Result<usize>]) -> Self {
            Self {
                results: RefCell::new(results.iter().copied().collect()),
                bytes: RefCell::new(vec![]),
            }
        }

        fn bytes(&self) -> Vec<u8> {
            self.bytes.borrow().clone()
        }
    }

    impl Sink for &FakeSink {
        fn write(&mut self, buffer: &[u8]) -> io::Result<usize> {
            let count = self
                .results
                .borrow_mut()
                .pop_front()
                .unwrap_or(Ok(buffer.len()))?;

            self.bytes.borrow_mut().extend(&buffer[..count]);

            Ok(count)
        }
    }

    fn write_bytes<const N: usize>(writer: &mut LineWriter<N, &FakeSink>, bytes: &[u8]) {
        for &byte in bytes {
            writer.write(byte).unwrap();
        }
    }

    #[test]
    fn write() {
        let sink = FakeSink::new(&[]);
        let mut writer = LineWriter::<4, _>::new(&sink);

        write_bytes(&mut writer, b"foo");

        assert_eq!(sink.bytes(), b"");
    }

    #[test]
    fn write_line() {
        let sink = FakeSink::new(&[]);
        let mut writer = LineWriter::<8, _>::new(&sink);

        write_bytes(&mut writer, b"foo\nbar");

        assert_eq!(sink.bytes(), b"foo\n");
    }

    #[test]
    fn write_lines() {
        let sink = FakeSink::new(&[]);
        let mut writer = LineWriter::<8, _>::new(&sink);

        write_bytes(&mut writer, b"foo\n\nbar\nbaz");

        assert_eq!(sink.bytes(), b"foo\n\nbar\n");
    }

    #[test]
    fn write_to_full_buffer() {
        let sink = FakeSink::new(&[]);
        let mut writer = LineWriter::<3, _>::new(&sink);

        write_bytes(&mut writer, b"foo");

        assert_eq!(sink.bytes(), b"");

        write_bytes(&mut writer, b"bar");

        assert_eq!(sink.bytes(), b"foo");

        write_bytes(&mut writer, b"baz");

        assert_eq!(sink.bytes(), b"foobar");
    }

    #[test]
    fn write_newline_to_full_buffer() {
        let sink = FakeSink::new(&[]);
        let mut writer = LineWriter::<3, _>::new(&sink);

        write_bytes(&mut writer, b"foo");

        assert_eq!(sink.bytes(), b"");

        write_bytes(&mut writer, b"\n");

        assert_eq!(sink.bytes(), b"foo\n");
    }

    #[test]
    fn write_line_to_full_buffer() {
        let sink = FakeSink::new(&[]);
        let mut writer = LineWriter::<4, _>::new(&sink);

        write_bytes(&mut writer, b"foo\nbar");

        assert_eq!(sink.bytes(), b"foo\n");

        write_bytes(&mut writer, b"\n");

        assert_eq!(sink.bytes(), b"foo\nbar\n");
    }

    #[test]
    fn flush_on_drop() {
        let sink = FakeSink::new(&[]);
        let mut writer = LineWriter::<8, _>::new(&sink);

        write_bytes(&mut writer, b"foo\nbar");
        drop(writer);

        assert_eq!(sink.bytes(), b"foo\nbar");
    }

    #[test]
    fn write_short() {
        let sink = FakeSink::new(&[Ok(1), Ok(2), Ok(1)]);
        let mut writer = LineWriter::<8, _>::new(&sink);

        write_bytes(&mut writer, b"foobar\n");

        assert_eq!(sink.bytes(), b"foobar\n");
    }

    #[test]
    fn write_interrupted() {
        let sink = FakeSink::new(&[Err(Errno::INTR), Ok(2), Err(Errno::INTR)]);
        let mut writer = LineWriter::<4, _>::new(&sink);

        write_bytes(&mut writer, b"foo\n");

        assert_eq!(sink.bytes(), b"foo\n");
    }

    #[test]
    fn write_zero() {
        let sink = FakeSink::new(&[Ok(2), Ok(0)]);
        let mut writer = LineWriter::<4, _>::new(&sink);

        write_bytes(&mut writer, b"foo");

        assert_eq!(writer.write(b'\n'), Err(Errno::IO));
        assert_eq!(sink.bytes(), b"fo");

        drop(writer);

        assert_eq!(sink.bytes(), b"foo\n");
    }

    #[test]
    fn fail_to_write() {
        let sink = FakeSink::new(&[Ok(2), Err(Errno::AGAIN)]);
        let mut writer = LineWriter::<8, _>::new(&sink);

        write_bytes(&mut writer, b"foo");

        assert_eq!(writer.write(b'\n'), Err(Errno::AGAIN));
        assert_eq!(sink.bytes(), b"fo");

        write_bytes(&mut writer, b"bar\n");

        assert_eq!(sink.bytes(), b"foo\nbar\n");
    }

    #[test]
    fn fail_to_write_to_full_buffer() {
        let sink = FakeSink::new(&[Err(Errno::AGAIN)]);
        let mut writer = LineWriter::<3, _>::new(&sink);

        write_bytes(&mut writer, b"foo");

        assert_eq!(writer.write(b'b'), Err(Errno::AGAIN));
        assert_eq!(sink.bytes(), b"");

        write_bytes(&mut writer, b"bar");

        assert_eq!(sink.bytes(), b"foo");

        drop(writer);

        assert_eq!(sink.bytes(), b"foobar");
    }

    #[test]
    fn ignore_error_on_drop() {
        let sink = FakeSink::new(&[Ok(1), Err(Errno::IO)]);
        let mut writer = LineWriter::<4, _>::new(&sink);

        write_bytes(&mut writer, b"foo");
        drop(writer);

        assert_eq!(sink.bytes(), b"f");
    }
}
