use super::source::Source;
use rustix::io::{self, retry_on_intr};

/// A reader that buffers bytes from a source.
#[derive(Debug)]
pub struct BufferedReader<S: Source, const N: usize> {
    source: S,
    buffer: [u8; N],
    index: usize,
    length: usize,
}

impl<S: Source, const N: usize> BufferedReader<S, N> {
    /// Creates a reader.
    pub const fn new(source: S) -> Self {
        Self {
            source,
            buffer: [0; N],
            index: 0,
            length: 0,
        }
    }

    /// Reads a byte.
    pub fn read(&mut self) -> io::Result<Option<u8>> {
        if self.index == self.length {
            self.length = retry_on_intr(|| self.source.read(&mut self.buffer))?;
            self.index = 0;
        }

        Ok(
            if let Some(&byte) = self.buffer[..self.length].get(self.index) {
                self.index += 1;

                Some(byte)
            } else {
                None
            },
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use alloc::collections::VecDeque;
    use rustix::io::Errno;

    struct FakeSource<'a> {
        results: VecDeque<io::Result<&'a [u8]>>,
    }

    impl<'a> FakeSource<'a> {
        fn new(results: &[io::Result<&'a [u8]>]) -> Self {
            Self {
                results: results.iter().copied().collect(),
            }
        }
    }

    impl Source for FakeSource<'_> {
        fn read(&mut self, buffer: &mut [u8]) -> io::Result<usize> {
            let bytes = self.results.pop_front().unwrap_or(Ok(&[]))?;

            buffer[..bytes.len()].copy_from_slice(bytes);

            Ok(bytes.len())
        }
    }

    #[test]
    fn read() {
        let mut reader = BufferedReader::<_, 4>::new(FakeSource::new(&[Ok(&[1, 2, 3])]));

        assert_eq!(reader.read(), Ok(Some(1)));
        assert_eq!(reader.read(), Ok(Some(2)));
        assert_eq!(reader.read(), Ok(Some(3)));
        assert_eq!(reader.read(), Ok(None));
        assert_eq!(reader.read(), Ok(None));
    }

    #[test]
    fn read_many() {
        let mut reader =
            BufferedReader::<_, 2>::new(FakeSource::new(&[Ok(&[1, 2]), Ok(&[3]), Ok(&[4, 5])]));

        assert_eq!(reader.read(), Ok(Some(1)));
        assert_eq!(reader.read(), Ok(Some(2)));
        assert_eq!(reader.read(), Ok(Some(3)));
        assert_eq!(reader.read(), Ok(Some(4)));
        assert_eq!(reader.read(), Ok(Some(5)));
        assert_eq!(reader.read(), Ok(None));
    }

    #[test]
    fn read_after_end() {
        let mut reader =
            BufferedReader::<_, 4>::new(FakeSource::new(&[Ok(&[1]), Ok(&[]), Ok(&[2])]));

        assert_eq!(reader.read(), Ok(Some(1)));
        assert_eq!(reader.read(), Ok(None));
        assert_eq!(reader.read(), Ok(Some(2)));
        assert_eq!(reader.read(), Ok(None));
    }

    #[test]
    fn read_interrupted() {
        let mut reader = BufferedReader::<_, 4>::new(FakeSource::new(&[
            Err(Errno::INTR),
            Err(Errno::INTR),
            Ok(&[1]),
        ]));

        assert_eq!(reader.read(), Ok(Some(1)));
        assert_eq!(reader.read(), Ok(None));
    }

    #[test]
    fn fail_to_read() {
        let mut reader =
            BufferedReader::<_, 4>::new(FakeSource::new(&[Ok(&[1, 2]), Err(Errno::IO), Ok(&[3])]));

        assert_eq!(reader.read(), Ok(Some(1)));
        assert_eq!(reader.read(), Ok(Some(2)));
        assert_eq!(reader.read(), Err(Errno::IO));
        assert_eq!(reader.read(), Ok(Some(3)));
        assert_eq!(reader.read(), Ok(None));
    }
}
