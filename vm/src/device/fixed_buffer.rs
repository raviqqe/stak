use super::Device;

#[derive(Debug)]
pub struct FixedBufferDevice<const I: usize, const O: usize> {
    input: [u8; I],
    output: [u8; O],
    input_index: usize,
    output_index: usize,
}

impl<const I: usize, const O: usize> FixedBufferDevice<I, O> {
    pub fn new() -> Self {
        Self {
            input: [0; I],
            output: [0; O],
            input_index: 0,
            output_index: 0,
        }
    }
}

impl<const I: usize, const O: usize> Default for FixedBufferDevice<I, O> {
    fn default() -> Self {
        Self::new()
    }
}

impl<const I: usize, const O: usize> Device for FixedBufferDevice<I, O> {
    type Error = ();

    fn read(&mut self) -> Result<u8, Self::Error> {
        if let Some(&byte) = self.input.get(self.input_index) {
            self.input_index += 1;

            Ok(byte)
        } else {
            Err(())
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
}
