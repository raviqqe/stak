mod decoder;

use crate::{decode::decoder::Decoder, Error, Program};

pub fn decode(codes: &[u8]) -> Result<Program, Error> {
    Decoder::new(codes).decode()
}
