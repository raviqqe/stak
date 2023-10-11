use crate::Device;

#[derive(Debug)]
pub struct FixedBufferDevice<const I: usize, const O: usize, const E: usize> {
    input: [u8; I],
    output: [u8; O],
    error: [u8; E],
    input_index: usize,
    output_index: usize,
    error_index: usize,
}

impl<const I: usize, const O: usize, const E: usize> FixedBufferDevice<I, O, E> {
    pub fn new() -> Self {
        Self {
            input: [0; I],
            output: [0; O],
            error: [0; E],
            input_index: 0,
            output_index: 0,
            error_index: 0,
        }
    }

    pub fn input_mut(&mut self) -> &mut [u8] {
        &mut self.input
    }

    pub fn output(&self) -> &[u8] {
        &self.output[..self.output_index]
    }

    pub fn error(&self) -> &[u8] {
        &self.error[..self.error_index]
    }
}

impl<const I: usize, const O: usize, const E: usize> Default for FixedBufferDevice<I, O, E> {
    fn default() -> Self {
        Self::new()
    }
}

impl<const I: usize, const O: usize, const E: usize> Device for FixedBufferDevice<I, O, E> {
    type Error = ();

    fn read(&mut self) -> Result<Option<u8>, Self::Error> {
        if let Some(&byte) = self.input.get(self.input_index) {
            self.input_index += 1;

            Ok(Some(byte))
        } else {
            Ok(None)
        }
    }

    fn write(&mut self, byte: u8) -> Result<(), Self::Error> {
        let Some(output) = self.output.get_mut(self.output_index) else {
            return Err(());
        };

        *output = byte;
        self.output_index += 1;

        Ok(())
    }

    fn write_error(&mut self, byte: u8) -> Result<(), Self::Error> {
        let Some(error) = self.error.get_mut(self.error_index) else {
            return Err(());
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
        let mut device = FixedBufferDevice::<1, 0, 0>::new();

        device.input_mut()[0] = 42;

        assert_eq!(device.read(), Ok(Some(42)));
        assert_eq!(device.read(), Ok(None));
    }

    #[test]
    fn write() {
        let mut device = FixedBufferDevice::<0, 1, 0>::new();

        assert_eq!(device.write(42), Ok(()));
        assert_eq!(device.write(42), Err(()));
        assert_eq!(device.output(), [42]);
    }

    #[test]
    fn write_error() {
        let mut device = FixedBufferDevice::<0, 0, 1>::new();

        assert_eq!(device.write_error(42), Ok(()));
        assert_eq!(device.write_error(42), Err(()));
        assert_eq!(device.error(), [42]);
    }
}
