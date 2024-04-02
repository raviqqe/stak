#[derive(Debug)]
pub struct ReadBuffer<'a> {
    data: &'a [u8],
    index: usize,
}

impl<'a> ReadBuffer<'a> {
    pub fn new(data: &'a [u8]) -> Self {
        Self { data, index: 0 }
    }

    pub fn read(&mut self) -> Option<u8> {
        if let Some(&byte) = self.data.get(self.index) {
            self.index += 1;

            Some(byte)
        } else {
            None
        }
    }
}
