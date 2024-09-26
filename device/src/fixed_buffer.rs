use crate::{BufferError, Device};

/// A fixed buffer device.
#[derive(Debug)]
pub struct FixedBufferDevice<'a, const O: usize, const E: usize> {
    input: &'a [u8],
    output: [u8; O],
    error: [u8; E],
    input_index: usize,
    output_index: usize,
    error_index: usize,
}

impl<'a, const O: usize, const E: usize> FixedBufferDevice<'a, O, E> {
    /// Creates a device.
    pub const fn new(input: &'a [u8]) -> Self {
        Self {
            input,
            output: [0; O],
            error: [0; E],
            input_index: 0,
            output_index: 0,
            error_index: 0,
        }
    }

    /// Returns a reference to standard output.
    pub fn output(&self) -> &[u8] {
        &self.output[..self.output_index]
    }

    /// Returns a reference to standard error.
    pub fn error(&self) -> &[u8] {
        &self.error[..self.error_index]
    }
}

impl<'a, const O: usize, const E: usize> Device for FixedBufferDevice<'a, O, E> {
    type Error = BufferError;

    fn read(&mut self) -> Result<Option<u8>, Self::Error> {
        Ok(if let Some(&byte) = self.input.get(self.input_index) {
            self.input_index += 1;

            Some(byte)
        } else {
            None
        })
    }

    fn write(&mut self, byte: u8) -> Result<(), Self::Error> {
        let Some(output) = self.output.get_mut(self.output_index) else {
            return Err(BufferError::Write);
        };

        *output = byte;
        self.output_index += 1;

        Ok(())
    }

    fn write_error(&mut self, byte: u8) -> Result<(), Self::Error> {
        let Some(error) = self.error.get_mut(self.error_index) else {
            return Err(BufferError::Write);
        };

        *error = byte;
        self.error_index += 1;

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn read() {
        let mut device = FixedBufferDevice::<0, 0>::new(&[42]);

        assert_eq!(device.read(), Ok(Some(42)));
        assert_eq!(device.read(), Ok(None));
    }

    #[test]
    fn write() {
        let mut device = FixedBufferDevice::<1, 0>::new(&[]);

        assert_eq!(device.write(42), Ok(()));
        assert_eq!(device.write(42), Err(()));
        assert_eq!(device.output(), [42]);
    }

    #[test]
    fn write_error() {
        let mut device = FixedBufferDevice::<0, 1>::new(&[]);

        assert_eq!(device.write_error(42), Ok(()));
        assert_eq!(device.write_error(42), Err(()));
        assert_eq!(device.error(), [42]);
    }
}
