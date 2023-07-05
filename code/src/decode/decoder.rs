use crate::{encode::INTEGER_BASE, Error, Program};

pub struct Decoder<'a> {
    codes: &'a [u8],
    index: usize,
}

impl<'a> Decoder<'a> {
    pub fn new(codes: &'a [u8]) -> Self {
        Self { codes, index: 0 }
    }

    pub fn decode(&mut self) -> Result<Program, Error> {
        todo!()
    }

    fn decode_integer(&mut self) -> u64 {
        let mut y = 0;

        while {
            y *= INTEGER_BASE;
            let x = self.decode_byte() as i8;

            y += (if x < 0 { -1 } else { 1 } * x) as u64;

            x < 0
        } {}

        y
    }

    fn decode_byte(&mut self) -> u8 {
        let byte = self.codes[self.index];
        self.index += 1;
        byte
    }
}
