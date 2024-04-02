#[derive(Debug)]
pub struct WriteBuffer<'a> {
    data: &'a mut [u8],
    index: usize,
}

impl<'a> WriteBuffer<'a> {
    pub fn new(data: &'a mut [u8]) -> Self {
        Self { data, index: 0 }
    }

    pub fn write(&mut self, byte: u8) -> bool {
        if let Some(pointer) = self.data.get_mut(self.index) {
            *pointer = byte;

            self.index += 1;

            false
        } else {
            true
        }
    }
}
